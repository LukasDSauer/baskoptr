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
  ewp <-
    do.call(pow, c(design = list(design), lambda = x[["lambda"]],
                   detail_params, weight_params = list(weight_params)))
  # Calculate FWER under global null
  detail_params$p1 <- rep(design@p0, design@k)
  fwer <-
    do.call(toer, c(design = list(design), lambda = x[["lambda"]], detail_params,
                    weight_params = list(weight_params)))
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
  ewp <-
    details$EWP
  # Calculate FWER under global null
  detail_params$p1 <- rep(design$p0, design$k)
  detailsfwer <- do.call(basksim::get_details,
                     c(design = list(design), as.list(x), detail_params))
  fwer <-
    detailsfwer$FWER
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
#' Utility function: Two-level family-wise power-error function
#'
#' This utility function is defined as
#' \deqn{u_{\text{2ewp}}(\boldsymbol\phi,\mathbf{p}) = \mathrm{ewp}(\boldsymbol
#' \phi,\mathbf{p}) - \left(\xi_1\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) +
#' \xi_2(\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta)\mathbf 1(
#' \mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta) \right)}
#' where \eqn{\eta\in[0,1]} is a threshold for imposing harder FWER penalty.
#'
#' @inheritParams u_ewp_discont
#' @param xi1 A numeric, `xi1` is the penalty for low FWER,
#' @param xi2 A numeric, `xi1 + xi2` is the penalty for high FWER,
#' @param thresh A numeric, for high FWER above this treshold we impose harder penalty.
#'
#' @return a numerical, the parameter combination's utility
#' @export
#'
#' @examples
#' design <- setupOneStageBasket(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_2ewp(design,
#'               x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'               detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    weight_fun = weights_fujikawa,
#'                                    logbase = exp(1)),
#'               xi1 = 1, xi2 = 2,
#'               thresh = 0.1)
u_2ewp <- function(design, x, detail_params, xi1, xi2, thresh) {
  weight_params <- list(epsilon = x[["epsilon"]], tau = x[["tau"]],
                        logbase = detail_params$logbase)
  detail_params <- detail_params[ - which(names(detail_params) == "logbase")]
  ewp <-
    do.call(pow, c(design = list(design), lambda = x[["lambda"]],
                   detail_params, weight_params = list(weight_params)))
  fwer <-
    do.call(toer, c(design = list(design), lambda = x[["lambda"]], detail_params,
                    weight_params = list(weight_params)))
  if(fwer > thresh){
    return(ewp - (xi1*fwer - xi2*(fwer - thresh)))
  } else{
    return(ewp - (xi1*fwer))
  }
}
#' Utility function: Scenario-averaged utility function
#'
#' For a utility function \eqn{u(\cdot, \mathbf p)} and a set of true scenarios
#' \eqn{\{\mathbf p_i,\ldots\}}, calculate the weighted average utility function
#' \deqn{ \bar u(x) = \sum_i w_i u(x, \mathbf p_i)}
#' for a set of weights with \eqn{\sum_i w_i=1}. By default, \eqn{w_i=\frac{1}{|\{\mathbf p_i,\ldots\}|}} for all \eqn{i}.
#'
#' @inheritParams u_ewp_discont
#' @param detail_params A named list of parameters that need to be supplied to
#'   `baskexact::toer()` and `baskexact::pow()`. It must not contain `p1`,
#'   as this is supplied separately.
#' @inheritParams opt_design_gen
#' @param p1s A numeric array in which each row defines a scenario of true response rates
#'  under the alternative hypothesis.
#' @param weights_u A numeric vector of weights for calculating the weighted average.

#'
#' @inherit u_ewp_discont return
#' @export
#'
#' @examples
#' design <- setupOneStageBasket(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
#' detail_params <- list(n = 20,
#'                       weight_fun = weights_fujikawa,
#'                       logbase = exp(1))
#' p1s <- rbind(c(0.2,0.2,0.2), c(0.2,0.2,0.5), c(0.2,0.5,0.5), c(0.5,0.5,0.5))
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_ewp_discont,
#'       utility_params = list(thresh = 0.05),
#'       p1s = p1s
#'       )
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_2ewp,
#'       utility_params = list(xi1 = 1, xi2 = 2, thresh = 0.1),
#'       p1s = p1s
#'       )
u_avg <- function(design, x, detail_params, utility, utility_params,
                  p1s, weights_u = rep(1/nrow(p1s), nrow(p1s))){
  u_fun <- function(p1){
    do.call(utility, c(design = list(design),
                       x = list(x),
                       detail_params = list(c(p1 = list(p1), detail_params)),
                       utility_params))}
  u_vals <- apply(X = p1s, MARGIN = 1, FUN = u_fun)
  return(sum(u_vals*weights_u))
}

