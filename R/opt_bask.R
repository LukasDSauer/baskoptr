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
#' @param trace A logical or a character string, should the trace of the
#' optimization algorithm be recorded? Default is `FALSE`. If `TRUE`, recording
#' is done by dynamic appending of a data frame (which may not be
#' very efficient). If `trace` is a character vector specifying a file path,
#' the trace will be dynamically saved to an RDS file.
#' Some optimization algorithms automatically return their
#' trace. In that case, you can switch off using `trace = FALSE` (or `""` or
#' `NULL`) and request the trace directly from your algorithm using
#' `algorith_params`.
#' @param x_names A character vector containing the names of the utility functions
#' tuning parameters, automatically retrieved from `algorithm_params$par` or
#' `algorithm_params$lower` if either is present. Default is `NULL`.
#' @param fn_name The name of the function argument of `algorithm`. The
#' default is `"fn"`.
#'
#' @return a list consisting of the algorithm's output (usually the optimal
#' parameter vector and the resulting optimal utility value and some meta
#' information). If `trace == TRUE`, the trace of the optimization algorithm
#' can be found in the `[["trace"]]` argument of the list. (This overwrites
#' any `[["trace"]]` argument in the algorithm's output.)
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
                           utility_params, algorithm_params, trace = FALSE,
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
  alg_trace <- data.frame()
  u_fun <- NULL
  trace_rec <- NULL
  # Should the trace be recorded and/or saved to a file?
  if(is.null(trace)){
    trace_rec <- "none"
  } else if(is.logical(trace)){
    trace_rec <- ifelse(trace, "return", "none")
  } else if(is.character(trace)){
    trace_rec <- ifelse(trace == "", "none", "return and save")
  }
  if(trace_rec == "return and save"){
    connection <- file(trace)
  }
  if(trace_rec == "none"){
    u_fun <- function(x){
      x_named <- x
      names(x_named) <- x_names
      return(do.call(utility, c(design = list(design),
                         x = list(x_named),
                         detail_params = list(detail_params),
                         utility_params)))}
  } else if(trace_rec == "return"){
    u_fun <- function(x){
      x_named <- x
      names(x_named) <- x_names
      fn <- do.call(utility, c(design = list(design),
                         x = list(x_named),
                         detail_params = list(detail_params),
                         utility_params))
      alg_trace <- cbind(alg_trace, rbind(x_named, fn))
      return(fn)
      }

  } else if(trace_rec == "return and save"){
    u_fun <- function(x){
      x_named <- x
      names(x_named) <- x_names
      fn <- do.call(utility, c(design = list(design),
                               x = list(x_named),
                               detail_params = list(detail_params),
                               utility_params))
      alg_trace <- cbind(alg_trace, rbind(x_named, fn))
      open(connection)
      saveRDS(alg_trace, con)
      close(connection)
      return(fn)
    }
  }
  args <- c(fn = u_fun,
            algorithm_params)
  names(args)[[1]] <- fn_name
  res <- do.call(algorithm, args)
  if(trace){
    res[["trace"]] <- alg_trace
  }
  return(res)
}

