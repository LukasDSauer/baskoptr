#' Extreme borrowing cutoff for epsilon dependent on tau
#'
#' @inheritParams u_ewp_discont
#' @param tau The optimization parameter \eqn{\tau} of Fujikawa's basket trial
#' design.
#' @param detail_params A named list of parameters containing per-stratum
#' sizes `n` and the base of the logarithm for calculating Jensen-Shannon
#' divergence `logbase`.
#'
#' @return A numeric, the extreme borrowing cutoff for \eqn{\varepsilon}.
#' @export
#'
#' @examples
#' design <- basksim::setup_fujikawa(k = 3,
#'                                  shape1 = 1,
#'                                  shape2 = 1,
#'                                  p0 = 0.2)
#' detail_params <- list(p1 = c(0.5, 0.2, 0.2),
#'                              n = 20,
#'                              logbase = exp(1))
#' epsilon_extreme(design, tau = 0.5, detail_params)
epsilon_extreme <- function(design, tau, detail_params){
  jsd_mat <- basksim:::get_weights_jsd(design,
                                      n = detail_params$n,
                                      epsilon = 1,
                                      tau = 0,
                                      logbase = detail_params$logbase)
  for(i in (1:(detail_params$n+1))){
    jsd_mat[i, i] <- -1
  }
  print(jsd_mat)
  max_weight <- max(jsd_mat)
  return(log(tau, base = max_weight))
}
