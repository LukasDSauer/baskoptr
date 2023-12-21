#' Class for Fujikawa's basket trial design.
#'
#' This is the generic class defining the basket trial design suggested by
#' Fujikawa et al., 2020.
#'
#' @slot tuning_params A list of real numbers of the form
#'   `list(lambda = , epsilon = , tau = )` denoting the tuning parameters.
#' @slot prior_params A list of two real numbers of the form
#'   `list(shape1 = , shape2 = )`
#'   denoting shape parameters `shape1` = $a$ and `shape2` = $b$ for the beta
#'   distribution $\mathrm{Beta}(a, b)$ which
#'   models the prior distribution of the response probability. So far, only
#'   independent identically distributed baskets are implemented. Real number
#'   vectors for `shape1` and `shape2` corresponding to individual prior
#'   distributions for different baskets may be implemented in the future.
#' @slot weights_fun A string denoting the name of the weights function. Default
#'   is `"jensen_shannon"`, meaning that the similarity of baskets is based on
#'   the Jensen-Shannon divergence.
#'
#' @examples
#' fujikawa(k = 3, n = 15,
#'          weights_fun = "jensen_shannon",
#'          prior_params = list(shape1 = 1, shape2 = 1),
#'          tuning_params = list(lambda = 0.99, epsilon = 2, tau = 0.5))
#' @export
fujikawa <- setClass("fujikawa",
                     slots = list(weights_fun = "character"),
                     contains = "basket_bayesian")
#' Decide which basket is active
#'
#' @inheritParams decide
#'
#' @export
setMethod(
  "decide",
  signature(design = "fujikawa", x = "numeric"),
  definition = function(design, x) {
    return(pbpost(x) >= design@tuning_params$lambda)
  }
)
#' Weights for borrowing in Fujikawa's design
#'
#' For a basket trial with `k` = `design@k` baskets, calculate the weights for
#' the borrowing mechanism in Fujikawa's design.
#' Depending on the specification of the `design@weights_fun`, different similarity
#' measures can be used.
#'
#' @return a matrix of dimension (`k`,`k`).
#' @include divergences.R
#' @export
setMethod(
  "bweights",
  signature(design = "fujikawa", x = "numeric"),
  definition = function(design, x) {
    if(design@weights_fun == "jensen_shannon"){
      basket_grid <- expand.grid((1:design@k), (1:design@k))
      bws <- with(basket_grid, jsdiv(function(s) { pbeta(q = s,
                                                         shape1 = design@prior_params$shape1[Var1],
                                                         shape2 = design@prior_params$shape2[Var1],
                                                         lower.tail = FALSE) },
                                     function(s) { pbeta(q = rate,
                                                         shape1 = design@prior_params$shape1[Var2],
                                                         shape2 = design@prior_params$shape2[Var2],
                                                         lower.tail = FALSE) },
                                     lower = 0, upper = 1))

    } else {
      stop(paste("The requested weight function", design@weights_fun,
                 "is unknown."))
    }
  }
)
#' Parameters of the posterior probability distributions
#'
#' @return a list of the form `list(shape1 = , shape2 = )`, containing two
#' vectors of length `k` denoting the shape parameters of the posterior
#' beta distributions in each basket.
#'
#' @export
setMethod(
  "post_params",
  signature(design = "fujikawa", x = "numeric"),
  definition = function(design, x) {
    return(list(shape1 = design@prior_params$shape1 + x,
                shape2 = design@prior_params$shape2 + design@n - x))
  }
)
#' Posterior probabilities (no borrowing)
#'
#' @export
setMethod(
  "ppost",
  signature(design = "fujikawa", x = "numeric"),
  definition = function(design, x) {

  }
)
#' Posterior probability with borrowing
#'
#' @export
setMethod(
  "pbpost",
  signature(design = "fujikawa", x = "numeric"),
  definition = function(design, x) {
    p_prms <- post_params(design, x)
    bws <- bweights(design, x)
    browser()
    mapply(function(rate, shape1, shape2) {
              return(pbeta(q = rate,
                    shape1 = shape1,
                    shape2 = shape2,
                    lower.tail = FALSE
                    ))
           },
           x / design@n,
           bws*pprms$shape1,
           bws*p_prms$shape2)
  }
)
