#' Utility functions: Discontinuous power/ECD functions with type-I error
#' penalty
#'
#' @inheritParams params_main
#' @param x  A named list, the design's tuning parameters to be optimized.
#' @param detail_params A named list of parameters that need to be supplied to
#' `get_details()`. If `p1` exists, `detail_params$p1` is replaced by `p1` for
#' calculating  power resp. ECD and by `p2` for calculating FWER.
#' @param p1 A numeric, response scenario for calculating power resp. ECD. This
#' can also be left `NULL`, then the function goes looking in
#' `detail_params$p1`.
#' @param p2 A numeric, response scenario for calculating FWER, default is the
#' global null scenario.
#' @param threshold A numeric, for high FWER above this threshold we impose a
#' penalty, default: 0.1.
#' @param penalty A numeric, the scaling factor for FWER penalty, default: 1.
#'
#' @return A numeric, the parameter combination's utility.
#' @export
#'
#' @examples
#' # Calculating the EWP utility using basksim as a backend
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_ewp(design,
#'       x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'       detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                            n = 20,
#'                            iter = 100,
#'                            logbase = exp(1)),
#'       penalty = 1, threshold = 0.1)
#' # Calculating the ECD utility using baskexact as a backend
#' design_x <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                        p0 = 0.2, backend = "exact")
#' u_ecd(design_x,
#'       x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'       detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                            n = 20,
#'                            weight_fun = baskexact::weights_fujikawa,
#'                            logbase = exp(1)),
#'       penalty = 1, threshold = 0.1)
u_ewp <- function(design, x, detail_params, p1 = NULL,
                  p2 = rep(design$p0, design$k), threshold, penalty) {
  details_list <- get_details_for_two_scenarios(design, x, detail_params, p1,
                                                p2)
  ewp <-
    details_list[["p1"]]$EWP
  fwer <-
    details_list[["p2"]]$FWER
  if (fwer >= threshold) {
    return(-fwer*penalty)
  } else{
    return(ewp)
  }
}

#' @rdname u_ewp
#' @export
u_ecd <- function(design, x, detail_params, p1 = NULL,
                  p2 = rep(design$p0, design$k),
                  penalty, threshold) {
  details_list <- get_details_for_two_scenarios(design, x, detail_params, p1,
                                                p2)
  ecd <-
    details_list[["p1"]]$ECD
  fwer <-
    details_list[["p2"]]$FWER
  if (fwer >= threshold) {
    return(-fwer*penalty)
  } else{
    return(ecd)
  }
}

#' Internal helper function: Get details for two response scenarios
#'
#' @inheritParams u_ewp
#' @return A list of two lists containing return values of `get_details` calls.
get_details_for_two_scenarios <- function(design, x, detail_params, p1, p2){
  # Calculate details under p1
  if(!is.null(p1)){
    detail_params$p1 <- p1
  } else if(is.null(detail_params$p1)){
    stop("You must supply either p1 or detail_params$p1!")
  }
  details_p1 <- do.call(baskwrap::get_details,
                      c(design = list(design), as.list(x), detail_params))

  # Calculate details under p2
  detail_params$p1 <- p2
  details_p2 <- do.call(baskwrap::get_details,
                      c(design = list(design), as.list(x), detail_params))
  return(list(p1 = details_p1,
              p2 = details_p2))
}

#' Utility functions: Two-level power-error combination functions
#'
#' These utility functions combine rewarding power and penalizing TOER by
#' subtracting TOER from power. In addition, unacceptably high FWER above a
#' certain threshold receives harsher penalty.
#'
#' The utility function \eqn{u_{\text{2ewp}}} is defined as
#' \deqn{u_{\text{2ewp}}(\boldsymbol\phi,\mathbf{p}) = \mathrm{ewp}(\boldsymbol
#' \phi,\mathbf{p}) - \left(\xi_1\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) +
#' \xi_2(\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta)\mathbf 1(
#' \mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta) \right)}
#' where \eqn{\eta\in[0,1]} is a threshold for imposing harsher FWER penalty.
#'
#' @inheritParams params_main
#' @param penalty1 A numeric, `penalty1` is the penalty for low FWER,
#' @param penalty2 A numeric, `penalty1 + penalty2` is the penalty for high
#' FWER.
#' @param threshold A numeric, for high FWER above this threshold we impose a
#' harsher penalty.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' u_2ewp(design,
#'        x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'        detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                             n = 20,
#'                             weight_fun = baskexact::weights_fujikawa,
#'                             logbase = exp(1)),
#'        penalty1 = 1, penalty2 = 2,
#'        threshold = 0.1)
#' u_2pow(design,
#'        x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'        detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                             n = 20,
#'                             weight_fun = baskexact::weights_fujikawa,
#'                             logbase = exp(1)),
#'        penalty1 = 1, penalty2 = 2,
#'        threshold = 0.1)
u_2ewp <- function(design, x, detail_params, penalty1, penalty2, threshold) {
  details <- do.call(baskwrap::get_details,
                     c(design = list(design), as.list(x), detail_params))
  ewp <-
    details$EWP
  fwer <- details$FWER
  if(fwer > threshold){
    return(ewp - (penalty1*fwer - penalty2*(fwer - threshold)))
  } else{
    return(ewp - (penalty1*fwer))
  }
}

#' @rdname u_2ewp
#' @export
u_2pow <- function(design, x, detail_params, penalty1, penalty2, threshold) {
  details <- do.call(baskwrap::get_details,
                     c(design = list(design), as.list(x), detail_params))
  alternative <- design$p0 != detail_params$p1
  return(sum(details$Rejection_Probabilities[alternative]) -
    penalty1*sum(details$Rejection_Probabilities[!alternative]) -
    penalty2*
      sum(details$Rejection_Probabilities[!alternative &
                                          (details$Rejection_Probabilities >
                                             threshold)] -
            threshold))

}

#' Utility function: Scenario-averaged utility function
#'
#' For a utility function \eqn{u(\cdot, \mathbf p)} and a set of true scenarios
#' \eqn{\{\mathbf p_i,\ldots\}}, calculate the weighted average utility function
#' \deqn{ \bar u(x) = \sum_i w_i u(x, \mathbf p_i)}
#' for a set of weights with \eqn{\sum_i w_i=1}. By default,
#' \eqn{w_i=\frac{1}{|\{\mathbf p_i,\ldots\}|}} for all \eqn{i}.
#'
#' @inheritParams params_main
#' @param detail_params A named list of parameters that need to be supplied to
#' `get_details()`. It must not contain `p1`,
#' as this is supplied separately.
#' @inheritParams params_utility_caller
#' @param p1s A numeric array in which each row defines a scenario of true
#' response rates under the alternative hypothesis.
#' @param weights_u A numeric vector of weights for calculating the weighted
#' average.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
#' detail_params <- list(n = 20,
#'                       weight_fun = baskexact::weights_fujikawa,
#'                       logbase = exp(1))
#' p1s <- rbind(c(0.2,0.2,0.2), c(0.2,0.2,0.5), c(0.2,0.5,0.5), c(0.5,0.5,0.5))
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = list(penalty = 1, threshold = 0.1),
#'       p1s = p1s
#'       )
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_2ewp,
#'       utility_params = list(penalty1 = 1, penalty2 = 2, threshold = 0.1),
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

#' Utility function with boundaries on the parameters
#'
#' This function manually implements boundaries for a given utility function
#' `utility`. If the vector `x` lies out of the lower and upper bounds, the
#' function returns `NA_real_`. Else it returns the utility functions value.
#'
#' @inheritParams params_main
#' @inheritParams params_utility_caller
#' @param lower numerical, a vector of lower bounds of the parameters.
#' @param upper numerical, a vector of upper bounds of the parameters.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' lower <- list(lambda = 0, epsilon = 1, tau = 0)
#' upper <- list(lambda = 1, epsilon = 10, tau = 1)
#' utility_params <- list(penalty = 1, threshold = 0.1)
#' detail_params <- list(p1 = c(0.5, 0.2, 0.2),
#'                       n = 20,
#'                       weight_fun = baskexact::weights_fujikawa,
#'                       logbase = exp(1))
#' # Out of bounds
#' u_bnd(design = design,
#'       x = list(lambda = 1.3, epsilon = 2, tau = 0.5),
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = utility_params,
#'       lower = lower,
#'       upper = upper)
#' # Inside bounds, this is the same as u_ewp
#' x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
#' u_bnd(design = design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = utility_params,
#'       lower = lower,
#'       upper = upper)
#' u_ewp(design = design,
#'       x = x,
#'       detail_params = detail_params, penalty = 1, threshold = 0.1)
u_bnd <-
  function(design, x, detail_params, utility, utility_params,
           lower, upper) {
    if (!all(as.numeric(lower) <= as.numeric(x)) |
        !(all(as.numeric(x) <= as.numeric(upper)))) {
      return(NA_real_)
    } else{
      return(do.call(utility, c(list(design = design,
                                     x = x,
                                     detail_params = detail_params),
                                utility_params)))
    }
  }
