#' Utility function: Discontinuous power function with type-I error penalty (exact)
#'
#' @param design An object of class `Basket` created by the function
#'   `baskexact::setupOneStageBasket`.
#' @param x  A named list of the design parameters to be optimized.
#' @param detail_params A named list of parameters that need to be supplied to
#'   `baskexact::toer()` and `baskexact::pow()`.
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
u_ewp_discont <- function(design, x, detail_params, thresh) {
  weight_params <- list(epsilon = x[["epsilon"]], tau = x[["tau"]],
                        logbase = detail_params$logbase)
  detail_params <- detail_params[ - which(names(detail_params) == "logbase")]
  fwer <-
    do.call(toer, c(design = list(design), lambda = x[["lambda"]], detail_params,
                    weight_params = list(weight_params)))
  ewp <-
    do.call(pow, c(design = list(design), lambda = x[["lambda"]],
                   detail_params, weight_params = list(weight_params)))
  if (fwer >= thresh) {
    return(-fwer)
  } else{
    return(ewp)
  }
}
#' Utility function: Discontinuous power function with type-I error penalty (simulated)
#'
#' @param design An object of class `fujikawa` created by the function
#'   `basksim::setup_fujikawa`.
#' @param x  A named list of the design parameters to be optimized.
#' @param detail_params A named list of parameters that need to be supplied to
#'   `basksim::get_details()`.
#'
#' @return a numerical, the parameter combination's utility
#' @export
#'
#' @examples
#' design <- basksim::setup_fujikawa(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_ewp_discont_sim(design,
#'               x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'               detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    iter = 1000,
#'                                    logbase = exp(1)),
#'               thresh = 0.05)
u_ewp_discont_sim <- function(design, x, detail_params, thresh) {
  details <- do.call(basksim::get_details,
                     c(design = list(design), as.list(x), detail_params))
  fwer <-
    details$FWER
  ewp <-
    details$EWP
  if (fwer >= thresh) {
    return(-fwer)
  } else{
    return(ewp)
  }
}
#' Utility function: Discontinuous power function with type-I error penalty and
#' boundaries on the parameters
#'
#' @inheritParams u_ewp_discont
#' @param lower numerical, a vector of lower bounds of the parameters.
#' @param upper numerical, a vector of upper bounds of the parameters.
#' @examples
#' design <- setupOneStageBasket(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_ewp_discont_bound(design,
#'               x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'               detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    weight_fun = weights_fujikawa,
#'                                    logbase = exp(1)),
#'               thresh = 0.05,
#'                   lower = list(lambda = 0, epsilon = 1, tau = 0),
#'                   upper = list(lambda = 1, epsilon = 10, tau = 1))
u_ewp_discont_bound <-
  function(design,
           x,
           detail_params,
           thresh,
           lower,
           upper) {
    if (!all(lower <= x) | !(all(x <= upper))) {
      return(NA_real_)
    } else{
      return(u_ewp_discont(design, x, detail_params, thresh))
    }
  }

