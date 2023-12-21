#' Decide which baskets are active
#'
#' Generic function for deciding which baskets are considered active
#' ("go" decision) and which baskets are considered inactive. The decision is
#' based on the number of responses in the baskets.
#'
#' @param design a basket trial design
#' @param x numeric vector of length `design@k` containing the number of
#' responses in each basket
#'
#' @return a logical vector of decisions for each basket
#'
#' @export
setGeneric("decide",
           def = function(design, x) { return(NULL) })
#' Weights for the borrowing mechanism of a basket trial
#'
#' Generic function for calculating weights for the borrowing mechanism of
#' a basket trial, based on the designs parameters and the outcomes in the
#' individual baskets
#'
#' @inheritParams ppost
#'
#' @return `NULL`
#'
#' @export
setGeneric("bweights",
           def = function(design, x) { return(NULL) })
#' Parameters of the posterior probability distributions
#'
#' Generic function for calculating posterior distributions in each basket.
#'
#' @inheritParams ppost
#' @return `NULL`
#'
#' @export
setGeneric(
  "post_params",
  def = function(design, x) {
    return(NULL)
  }
)
#' Posterior probability
#'
#' Generic function for calculating the posterior probability that the true rate
#' in each basket is above a certain threshold, i.e.
#' $$ P(\theta_k > \theta_k^* | R_k = r_k).$$
#'
#' @param design a basket trial design
#' @param x numeric vector of length `design@k` containing the number of
#' responses in each basket
#' @return `NULL`
#'
#' @seealso pbpost
#' @export
setGeneric(
  "ppost",
  def = function(design, x) {
    return(NULL)
  }
)
#' Posterior probability
#'
#' Generic function for calculating the "altered posterior probability", i.e. a
#' probability that the true rate in each basket is above a certain threshold,
#' $$ \tilde P(\theta_k > \theta_k^* | R_k = r_k),$$
#' where the probability density function/probability mass function implements
#' some borrowing mechanism between similar baskets.
#'
#' @inheritParams ppost
#' @return `NULL`
#'
#' @seealso ppost
#' @export
setGeneric(
  "pbpost",
  def = function(design, x) {
    return(NULL)
  }
)
