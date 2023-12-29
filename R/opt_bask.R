#' Optimize a Basket Trial Design
#'
#' Optimize the parameters of a basket trial design using a utility-based
#' approach with a simulation algorithm of your choice.
#'
#' @inheritParams u_ewp_discont
#' @param utility A function returning the utility of a parameter combination
#' `x`, of the form `utility(design, x, detail_params, ...)`, where `...` may
#' further parameters to be supplied to the utility.
#'
#' @param algorithm A function returning the optimization algorithm's results,
#' should have the form `algorithm(fn, ...)`, where `fn` is the
#' function to be optimized and `...` may be further parameters of the
#' optimization algorithm.
#' @param utility_params A named list of further parameters that need to be
#' supplied to  the utility function.
#' @param algorithm_params A named list of further parameters that need to be
#' supplied to the optimization algorithm.
#' @param trace A logical, should the trace of the optimization algorithm be
#' returned?
#'
#' @return a list consisting of the optimal parameter vector, the resulting
#' optimal utility value, and the trace of the optimization algorithm.
#'
#' @export
#'
#' @examples
#' design <- setupOneStageBasket(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' opt_design_gen(design = design,
#'                utility = u_ewp_discont,
#'                algorithm = optimizr::simann,
#'                detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    weight_fun = weights_fujikawa,
#'                                    logbase = exp(1)),
#'                utility_params = list(thresh = 0.05),
#'                algorithm_params = list(par = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        lower = c(lambda = 0.001,
#'                                                     epsilon = 1,
#'                                                     tau = 0.001),
#'                                        upper = c(lambda = 0.999,
#'                                                  epsilon = 10,
#'                                                  tau = 0.999),
#'                                        control = list(maxit = 10000,
#'                                                       temp = 2000,
#'                                                       fnscale = -1)))
#' opt_design_gen(design = design,
#'                utility = u_powfwer_discont_bound,
#'                algorithm = stats_optim_sann,
#'                detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                     logbase = exp(1), exact = TRUE),
#'                utility_params = list(alpha = 0.05,
#'                                      lower = c(lambda = 0.001,
#'                                                epsilon = 1,
#'                                                tau = 0.001),
#'                                       upper = c(lambda = 0.999,
#'                                                 epsilon = 10,
#'                                                 tau = 0.999)),
#'                algorithm_params = list(start = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        maximization = TRUE,
#'                                        control = list(maxit = 30000,
#'                                                       temp = 2000,
#'                                                       REPORT = 2)))
opt_design_gen <- function(design, utility, algorithm, detail_params,
                           utility_params, algorithm_params, trace = TRUE){
  x_names <- character()
  if(!is.null(algorithm_params$lower)){
    x_names <- names(algorithm_params$lower)
  } else if(!is.null(algorithm_params$par)){
    x_names <- names(algorithm_params$par)
  } else {
    stop("Cannot retrieve parameter vector names from algorithm_params. Please
         supply a 'lower' or a 'par'  argument in algorithm_params.")
  }
  u_fun <- function(x){
    x_named <- x
    names(x_named) <- x_names
    do.call(utility, c(design = list(design),
                       x = list(x_named),
                       detail_params = list(detail_params),
                       utility_params))}
  res <- do.call(algorithm,
                 c(fn = u_fun,
                   algorithm_params))
}
