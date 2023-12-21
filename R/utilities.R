#' Utility function: Discontinuous power function with type-I error penalty
#'
#' @template design
#' @template x
#' @template detail_params
#'
#' @return a numerical, the parameter combination's utility
#' @export
#'
#' @examples
#' design <- setupOneStageBasket(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_ewp_discont(design,
#'               x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'               detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    weight_fun = weights_fujikawa,
#'                                    logbase = exp(1)),
#'               thresh = 0.05)
u_ewp_discont <- function(design, x, detail_params, thresh){
  fwer <- do.call(toer, c(design = list(design), as.list(x), detail_params))
  ewp <- do.call(pow, c(design = list(design), as.list(x), detail_params))
  if(fwer >= thresh){
    return(-fwer)
  } else{
    return(ewp)
  }
}
#' Utility function: Discontinuous power function with type-I error penalty and
#' boundaries on the parameters
#'
#' @template lower
#' @template upper
#'
#' @inheritParams u_powfwer_discont
#' @inherit u_powfwer_discont return
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2, shape1 = 1, shape2 = 1)
#' u_ewp_discont_bound(design,
#'                   x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'                   detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                        logbase = exp(1), exact = TRUE),
#'                   alpha = 0.05,
#'                   lower = list(lambda = 0, epsilon = 1, tau = 0),
#'                   upper = list(lambda = 1, epsilon = 10, tau = 1))
u_ewp_discont_bound <- function(design, x, detail_params, alpha, lower,
                                    upper){
  if(!all(lower < x) | !(all(x < upper))){
    return(NA_real_)
  } else{
    return(u_powfwer_discont(design, x, detail_params, alpha))
  }
}
