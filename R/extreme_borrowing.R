#' Extreme borrowing cutoffs for borrowing in Fujikawa's design
#'
#' The weights for the borrowing posterior in Fujikawa's design are calculated
#' as
#' \deqn{\omega_{ij} = \mathbf{1}(\tilde\omega_{ij}^\varepsilon>\tau)\cdot\tilde\omega_{ij}^\varepsilon}
#' with \eqn{\tau\in[0,1]}, \eqn{\varepsilon \geq 0}, and
#' \eqn{\tilde\omega_{ij}= 1 - JS(\mathrm{Beta}_i^{\text{post}}, \mathrm{Beta}_j^{\text{post}})}.
#' Here, \eqn{JS(\cdot,\cdot)} is the Jensen-Shannon divergence, and
#' \deqn{\mathrm{Beta}_i^{\text{post}}=\mathrm{Beta}(a_i+r_i, b_i+n_i-r_i)}
#' is the beta-binomial conjugated posterior distribution in  the \eqn{i}-th stratum
#' for prior Beta parameters \eqn{a_i} an \eqn{b_i}, number of responses
#' \eqn{r_i} and number of patients \eqn{n_i}. In particular for
#' \deqn{\tilde\omega_{ij}^\varepsilon\leq\tau,}
#' borrowing will only take place for baskets with completely identical response rates.
#' We call this boundary the *extreme borrowing cutoff*. The functions
#' `epsilon_extreme` and `tau_extreme` calculate the extreme borrowing cutoffs
#' \eqn{\tau_{\text{extreme}}(\varepsilon)} and
#' \eqn{\varepsilon_{\text{extreme}}(\tau)}, which are the boundaries of
#' \deqn{  \tau \geq (\max_{r_k\neq r_l}\tilde\omega_{kl})^\varepsilon }
#' and
#' \deqn{   \varepsilon \geq \log_{\max_{r_k\neq r_l}\tilde\omega_{kl}}(\tau).}
#'
#' The extreme borrowing cutoffs depend neither on `p0` nor on `p1`. They only
#' depend on the number of baskets `k` and the number of patients per basket `n`.
#'
#' @inheritParams u_ewp_discont
#' @param epsilon The optimization parameter \eqn{\varepsilon} of Fujikawa's basket trial
#' design.
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
#' detail_params <- list(n = 20,
#'                       logbase = exp(1))
#' epsilon_extreme(design, tau = 0.5, detail_params)
#' tau_extreme(design, epsilon = 2, detail_params)
epsilon_extreme <- function(design, tau, detail_params){
  jsd_mat <- basksim:::get_weights_jsd(design,
                                      n = detail_params$n,
                                      epsilon = 1,
                                      tau = 0,
                                      logbase = detail_params$logbase)
  # ignore the diagonal, as it concerns the case k = l and were are only
  # interested in k neq l.
  for(i in (1:(detail_params$n+1))){
    jsd_mat[i, i] <- -1
  }
  max_weight <- max(jsd_mat)
  return(log(tau, base = max_weight))
}
#' @rdname epsilon_extreme
#' @export
tau_extreme <- function(design, epsilon, detail_params){
  jsd_mat <- basksim:::get_weights_jsd(design,
                                       n = detail_params$n,
                                       epsilon = 1,
                                       tau = 0,
                                       logbase = detail_params$logbase)
  for(i in (1:(detail_params$n+1))){
    jsd_mat[i, i] <- -1
  }
  max_weight <- max(jsd_mat)
  return(max_weight^epsilon)
}
