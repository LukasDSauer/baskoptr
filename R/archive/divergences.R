#' Kullback-Leibler divergence of two continuous probability density functions
#'
#' The Kullback-Leibler divergence of two continuous probability density
#' functions $p(x)$ and $q(x)$ on the domain $[l, u]$ is calculated via
#' the usual formula
#' $$ D_{\mathrm{KL}}(p, q) = \int_l^u p(x)\log(\frac{p(x)}{q(x)}) dx.$$
#'
#' @param p a continuous probability density function
#' @param q another continuous probability density function
#' @param lower lower boundary of the domain of `p()` and `q()`
#' @param upper lower boundary of the domain of `p()` and `q()`
#'
#' @return a numeric value, the Kullback-Leibler divergence of `p()` and `q()`
#' @export
#'
#' @examples
kldiv <- function(p, q, lower, upper){
  return(stats::integrate(function(x) {p(x) * log(p(x)/q(x))},
                   lower = lower,
                   upper = upper))
}
#' Jensen-Shannon divergence of two continuous probability density functions
#'
#' The Jensen-Shannon divergence of two continuous probability density
#' functions $p(x)$ and $q(x)$ on the domain $[l, u]$ is calculated via
#' the usual formula
#' $$ D_{\mathrm{JS}}(p, q) = \frac{1}{2}(D_{\mathrm{KL}}(p, m) + D_{\mathrm{KL}}(q, m)),$$
#'
#' where $m(x) = \frac{1}{2}(p(x) + q(x))$ and $D_{\mathrm{KL}}$ is the
#' Kullback-Leibler divergence.
#'
#' @inheritParams kldiv
#'
#' @return a numeric value, the Kullback-Leibler divergence of `p()` and `q()`
#' @export
#'
#' @examples
jsdiv <- function(p, q, lower, upper){
  m <- function(x) { p(x) + q(x) }
  return(0.5 * (kldiv(p, m, lower, upper) + kldiv(q, m, lower, upper)))
}
