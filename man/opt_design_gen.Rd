% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt_bask.R
\name{opt_design_gen}
\alias{opt_design_gen}
\title{Optimize a Basket Trial Design}
\usage{
opt_design_gen(
  design,
  utility,
  algorithm,
  detail_params,
  utility_params,
  algorithm_params,
  x_names = NULL,
  fn_name = "fn",
  trace = FALSE,
  trace_options = list(robust = FALSE),
  format_result = NULL,
  final_details = FALSE,
  final_details_utility_params = utility_params,
  progress_bar = NULL
)
}
\arguments{
\item{design}{An object of class \code{fujikawa} created by the function
\code{baskwrap::setup_fujikawa_x} or \code{basksim::setup_fujikawa}.}

\item{utility}{A function returning the utility of a parameter combination
\code{x} of the form \code{utility(design, x, detail_params, ...)}, where \code{...} may
further parameters to be supplied to the utility.}

\item{algorithm}{A function returning the optimization algorithm's results,
should have the form \code{algorithm(fn, ...)}, where \code{fn} is the
function to be optimized and \code{...} may be further parameters of the
optimization algorithm.}

\item{detail_params}{A named list of parameters that need to be supplied to
\code{get_details()}.}

\item{utility_params}{A named list of further parameters that need to be
supplied to  the utility function.}

\item{algorithm_params}{A named list of further parameters that need to be
supplied to the optimization algorithm.}

\item{x_names}{A character vector containing the names of the utility functions
tuning parameters, automatically retrieved from \code{algorithm_params$par} or
\code{algorithm_params$lower} if either is present. Default is \code{NULL}.}

\item{fn_name}{The name of the function argument of \code{algorithm}. The
default is \code{"fn"}.}

\item{trace}{A logical or a character string, should the trace of the
optimization algorithm be recorded (utility values, parameters vectors,
and random number seeds)? Default is \code{FALSE}. If \code{TRUE}, recording
is done by dynamic appending of a data frame (which may not be
very efficient). If \code{trace} is a character vector specifying a file path
(ending with \code{".RDS"}), the trace will be dynamically saved to that RDS file.
Some optimization algorithms automatically return their
trace. In that case, you can switch off using \code{trace = FALSE} (or \code{""} or
\code{NULL}) and request the trace directly from your algorithm using
\code{algorith_params}.}

\item{trace_options}{A list, if \code{trace_options = list(robust = TRUE)}, a
robust recording method for saving the optimization steps is used that works
well with parallelized algorithms. Caveat: the robust version is not sorted
in chronological order.}

\item{format_result}{(Optional:) A function \verb{function(res)} for formatting
the final output of the optimization algorithm. If you want
\code{final_details = TRUE}, then the formatted result must have a non-null element
\code{res$par} containing the optimal parameters.}

\item{final_details}{A logical, if \code{TRUE}, the function runs the
utility function one more time on the optimization result and returns the
output of the implicit call to \code{baskwrap::get_details()} as
\code{attr(, "final_details")} and the result of the repeated utility function call
as \code{attr(, "final_res_repeated")}. The latter should be identical to the
result for deterministic calculations, but may differ for stochastic
calculations.}

\item{final_details_utility_params}{A list, only takes effect if
\code{final_details==TRUE}. This last run of the utility function will
use these list of parameters instead of \code{utility_params}. However, the
default is \code{final_details_utility_params = utility_params}.}

\item{progress_bar}{Do you want a progress bar? This argument either takes
a numeric containing the number of steps or a \code{progressr::progressor()}. Only
works if \code{trace} is activated. The number of steps is proportional to the
number of algorithm iterations, but the exact number depends on the
algorithm's implementation.}
}
\value{
a list consisting of the algorithm's output (usually the optimal
parameter vector and the resulting optimal utility value and some meta
information). If \code{trace == TRUE}, the trace of the optimization algorithm
can be found in the \verb{[["trace"]]} entry of the list. (If the algorithm
function also outputs a trace, this will be saved in an additional
\verb{[["trace_alg"]]} entry.)
}
\description{
Optimize the parameters of a basket trial design using a utility-based
approach with a simulation algorithm of your choice.
}
\examples{
# Optimizing a three-basket trial design using Fujikawa's beta-binomial
# sharing approach
design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
                                     p0 = 0.2, backend = "exact")
detail_params <- list(p1 = c(0.5, 0.2, 0.2),
                                    n = 20,
                                    weight_fun = baskexact::weights_fujikawa,
                                    logbase = exp(1))
utility_params <- list(penalty = 1, thresh = 0.1)
# Bounded simulated annealing with progress bar
progressr::handlers(global = TRUE)
opt_design_gen(design = design,
               utility = u_ewp,
               algorithm = optimizr::simann,
               detail_params = detail_params,
               utility_params = utility_params,
               algorithm_params = list(par = c(lambda = 0.99,
                                               epsilon = 2,
                                               tau = 0.5),
                                       lower = c(lambda = 0.001,
                                                 epsilon = 1,
                                                 tau = 0.001),
                                       upper = c(lambda = 0.999,
                                                 epsilon = 10,
                                                 tau = 0.999),
                                       control = list(maxit = 10,
                                                      temp = 10,
                                                      fnscale = -1)),
                                                      trace = TRUE,
                progress_bar = 10 + 2)
}
