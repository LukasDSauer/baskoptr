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
#' @param x_names A character vector containing the names of the utility functions
#' tuning parameters, automatically retrieved from `algorithm_params$par` or
#' `algorithm_params$lower` if either is present. Default is `NULL`.
#' @param fn_name The name of the function argument of `algorithm`. The
#' default is `"fn"`.
#' @param trace A logical or a character string, should the trace of the
#' optimization algorithm be recorded (utility values, parameters vectors,
#' and random number seeds)? Default is `FALSE`. If `TRUE`, recording
#' is done by dynamic appending of a data frame (which may not be
#' very efficient). If `trace` is a character vector specifying a file path
#' (ending with `".RDS"`), the trace will be dynamically saved to that RDS file.
#' Some optimization algorithms automatically return their
#' trace. In that case, you can switch off using `trace = FALSE` (or `""` or
#' `NULL`) and request the trace directly from your algorithm using
#' `algorith_params`.
#' @param format_result (Optional:) A function `function(res)` for formatting
#' the final output of the optimization algorithm.
#' @param final_details A logical, if `TRUE`, the function runs the
#' utility function one more time on the optimization result and returns the
#' output of the implicit call to `baskwrap::get_details()` as
#' `attr(, "final_details")` and the result of the repeated utility function call
#' as `attr(, "final_res_repeated")`. The latter should be identical to the
#' result for deterministic calculations, but may differ for stochastic
#' calculations.
#' @param final_details_utility_params A list, only takes effect if
#' `final_details==TRUE`. This last run of the utility function will
#' use these list of parameters instead of `utility_params`. However, the
#' default is `final_details_utility_params = utility_params`.
#'
#'
#' @return a list consisting of the algorithm's output (usually the optimal
#' parameter vector and the resulting optimal utility value and some meta
#' information). If `trace == TRUE`, the trace of the optimization algorithm
#' can be found in the `[["trace"]]` entry of the list. (If the algorithm
#' function also outputs a trace, this will be saved in an additional
#'  `[["trace_alg"]]` entry.)
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
                           utility_params, algorithm_params,
                           x_names = NULL, fn_name = "fn",
                           trace = FALSE,
                           format_result = NULL,
                           final_details = FALSE,
                           final_details_utility_params = utility_params){
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
  # Should the trace/the seeds be recorded and/or saved to a file?
  trace_info <- get_trace_info(trace)
  trace_rec <- trace_info[["rec"]]
  trace_path <- trace_info[["path"]]

  if(trace_rec == "none"){
    u_fun <- function(x){
      x_named <- x
      names(x_named) <- x_names
      return(do.call(utility, c(design = list(design),
                         x = list(x_named),
                         detail_params = list(detail_params),
                         utility_params)))}
  } else if(trace_rec == "return" | trace_rec == "return and save"){
    if(!file.exists(trace_path)){
      file.create(trace_path)
    }
    saveRDS(NULL, trace_path)
    u_fun <- function(x){
      x_named <- x
      names(x_named) <- x_names
      seed <- .Random.seed
      names(seed) <- paste0("seed", (1:length(seed)))
      fn <- do.call(utility, c(design = list(design),
                               x = list(x_named),
                               detail_params = list(detail_params),
                               utility_params))
      alg_trace <- readRDS(trace_path)
      saveRDS(rbind(alg_trace, cbind(t(x_named), fn, t(seed))), trace_path)
      return(fn)
    }
  }
  args <- c(fn = u_fun,
            algorithm_params)
  names(args)[[1]] <- fn_name
  res <- do.call(algorithm, args)
  if(!is.null(format_result)){
    res <- format_result(res)
  }
  if(trace_rec %in% c("return", "return and save")){
    if(!is.null(res[["trace"]])){
      res[["trace_alg"]] <- res[["trace"]]
    }
    res[["trace"]] <- readRDS(trace_path)
  }
  if(trace_rec == "return"){
    file.remove(trace_path)
  }
  if(final_details){
    final_details_utility_params$report_details <- TRUE
    res_named <- res$par
    names(res_named) <- x_names
    res_repeated <- do.call(utility, c(design = list(design),
                                       x = list(res_named),
                                       detail_params = list(detail_params),
                                       final_details_utility_params))
    attr(res, "final_details") <- attr(res_repeated, "details")
    attributes(res_repeated) <- NULL
    attr(res, "final_res_repeated") <- res_repeated
  }
  return(res)
}

