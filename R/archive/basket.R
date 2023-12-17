#' Class for a generic basket trial design.
#'
#' This is the generic class defining a basket trial design. A basket trial
#' design is study design for single-arm studies with binary endpoints which are
#' subdivided into `k` disease subtypes, the so-called baskets, with `n`
#' individuals per basket.
#'
#' @slot k A positive integer denoting the number of baskets.
#' @slot n A positive integer denoting the number of individuals per basket.
#' @slot tuning_params A list of real numbers denoting parameters for tuning the
#'   design.
#'
#' @include generics.R
#' @export
basket <- setClass("basket",
         slots=list(k = "numeric",
                    n = "numeric",
                    tuning_params = "numeric"))
setMethod("decide",
          signature(x = "basket"))


