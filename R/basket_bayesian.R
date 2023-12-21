#' Class for a Bayesian basket trial design.
#'
#' This is the generic class defining a Bayesian basket trial design. In a
#' Bayesian basket trial design, the response probabilities
#' in the individual baskets are modeled on a prior distribution. The decision
#' mechanism for stopping and continuing baskets and the borrowing mechanism
#' for sharing information between baskets are also based on the Bayesian
#' framework, e.g. borrowing is implemented by taking the posterior distribution
#' into account.
#'
#' @slot prior_params A list of real numbers denoting shape parameters for
#'   defining the prior distribution of the responses or response rates.
#'
#'
#' @seealso basket fujikawa
#' @export
basket_bayesian <- setClass("basket_bayesian",
         slots=list(prior_params = "list"),
         contains = "basket")
