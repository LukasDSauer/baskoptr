#'  List of parameters used across the package
#'
#' @name params_main
#' @param design An object of class `fujikawa` created by the function
#' `baskwrap::setup_fujikawa_x` or `basksim::setup_fujikawa`.
#' @param x  A named list, the design's tuning parameters to be optimized.
#' @param detail_params A named list of parameters that need to be supplied to
#' `get_details()`.
NULL

#'  List of parameters used for functions calling utility functions
#'
#' @name params_utility_caller
#' @param utility A function returning the utility of a parameter combination
#' `x` of the form `utility(design, x, detail_params, ...)`, where `...` may
#' further parameters to be supplied to the utility.
#' @param utility_params A named list of further parameters that need to be
#' supplied to  the utility function.
NULL
