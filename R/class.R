# Parent-class Basket for internal use only.
#' @import baskexact
setClass("Basket", contains = "Basket")

#' Class OneStageBasket
#' @import baskexact
setClass("OneStageBasket", contains = "OneStageBasket")

#' Class TwoStageBasket
#' @import baskexact
setClass("TwoStageBasket", contains = "TwoStageBasket")
