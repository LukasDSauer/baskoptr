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
