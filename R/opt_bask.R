#' Optimize a Basket Trial Design
#'
#' Optimize the parameters of a basket trial design using a utility-based
#' approach with a simulation algorithm of your choice.
#'
#' @inheritParams params_main
#' @inheritParams params_utility_caller
#'
#' @param algorithm A function returning the optimization algorithm's results,
#' should have the form `algorithm(fn, ...)`, where `fn` is the
#' function to be optimized and `...` may be further parameters of the
#' optimization algorithm.
#' @param algorithm_params A named list of further parameters that need to be
#' supplied to the optimization algorithm.
#' @param trace A logical, should the trace of the optimization algorithm be
#' returned?
#' @param x_names A character vector containing the names of the utility functions
#' tuning parameters, automatically retrieved from `algorithm_params$par` or
#' `algorithm_params$lower` if either is present. Default is `NULL`.
#' @param fn_name The name of the function argument of `algorithm`. The
#' default is `"fn"`.
#'
#' @return a list consisting of the optimal parameter vector, the resulting
#' optimal utility value, and the trace of the optimization algorithm.
#'
#' @export
#'
#' @examples
#' # Optimizing a three-basket trial design using Fujikawa's beta-binomial
#' # sharing approach
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' detail_params <- list(p1 = c(0.5, 0.2, 0.2),
#'                                     n = 20,
#'                                     weight_fun = baskexact::weights_fujikawa,
#'                                     logbase = exp(1))
#' utility_params <- list(penalty = 1, thresh = 0.1)
#' # Bounded simulated annealing
#' opt_design_gen(design = design,
#'                utility = u_ewp,
#'                algorithm = optimizr::simann,
#'                detail_params = detail_params,
#'                utility_params = utility_params,
#'                algorithm_params = list(par = c(lambda = 0.99,
#'                                                epsilon = 2,
#'                                                tau = 0.5),
#'                                        lower = c(lambda = 0.001,
#'                                                  epsilon = 1,
#'                                                  tau = 0.001),
#'                                        upper = c(lambda = 0.999,
#'                                                  epsilon = 10,
#'                                                  tau = 0.999),
#'                                        control = list(maxit = 10,
#'                                                       temp = 10,
#'                                                       fnscale = -1)))
opt_design_gen <- function(design, utility, algorithm, detail_params,
                           utility_params, algorithm_params, trace = TRUE,
                           x_names = NULL, fn_name = "fn"){
  if(is.null(x_names)){
    if(!is.null(algorithm_params$lower)){
      x_names <- names(algorithm_params$lower)
    } else if(!is.null(algorithm_params$par)){
      x_names <- names(algorithm_params$par)
    } else {
      stop("Cannot retrieve parameter vector names from algorithm_params and
          x_names is NULL. Please supply an x_names argument or
          algorithm_params$par or algorithm_params$lower.")
    }
  }
  u_fun <- function(x){
    x_named <- x
    names(x_named) <- x_names
    print(x_named)
    do.call(utility, c(design = list(design),
                       x = list(x_named),
                       detail_params = list(detail_params),
                       utility_params))}
  args <- c(fn = u_fun,
            algorithm_params)
  names(args)[[1]] <- fn_name
  res <- do.call(algorithm, args)
  return(res)
}

