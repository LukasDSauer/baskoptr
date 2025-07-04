#' Utility functions: Discontinuous power/ECD functions with type-I error
#' penalty
#'
#' These utility functions return experiment-wise power (EWP) or expected number
#' of
#' correct decisions (ECD) if the family-wise error rate (FWER) is low and the
#' negative FWER multiplied by a penalty parameter if the FWER is high.
#'
#' The utility function `u_ewp` is defined as
#' defined as
#' \deqn{u_{\text{ewp}}(x,\mathbf{p_1},\mathbf{p_2})=\mathrm{ewp}
#' (x,\mathbf{p_1}),}
#' if the FWER fulfills \eqn{\mathrm{fwer}(x,\mathbf{p_2}) < \eta_1}, and
#' \deqn{u_{\text{ewp}}(x,\mathbf{p_1},\mathbf{p_2})=
#' -\xi_1\cdot\mathrm{fwer}(x,\mathbf{p_2}),}
#' if \eqn{\mathrm{fwer}(x,\mathbf{p_2}) \geq \eta_1}. The parameter
#' \eqn{\eta_1} is called the threshold, the parameter \eqn{\xi_1} is called
#' the penalty.
#'
#' The utility function `u_ecd` is defined analogously with the expected number
#' of correct decisions instead of the experiment-wise power.
#'
#' @inheritParams params_main
#' @param x  A named list, the design's tuning parameters to be optimized.
#' @param detail_params A named list of parameters that need to be supplied to
#' `get_details()`. If `p1` exists, `detail_params$p1` is replaced by `p1` for
#' calculating  power resp. ECD and by `p2` for calculating FWER.
#' @param p1 A numeric, response scenario for calculating power resp. ECD. This
#' can also be left `NULL`, then the function goes looking in
#' `detail_params$p1`.
#' @param p2 A numeric, response scenario for calculating FWER, default is the
#' global null scenario.
#' @param threshold A numeric, for high FWER above this threshold we impose a
#' penalty, default: 0.1.
#' @param penalty A numeric, the scaling factor for FWER penalty, default: 1.
#' @param reduce_calculations A logical, only takes effect for the
#' `"exact"` backend. If `TRUE`, the function will only execute the
#' `get_details()` function for `p1` if the FWER for `p2` turned out
#' to be low enough. This may speed up function execution.
#' Default is `TRUE` for the `"exact"` backend and `FALSE` otherwise.
#'
#' @return A numeric, the parameter combination's utility.
#' @export
#'
#' @examples
#' # Calculating the EWP utility using basksim as a backend
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
#' u_ewp(design,
#'       x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'       detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                            n = 20,
#'                            iter = 100,
#'                            logbase = exp(1)),
#'       penalty = 1, threshold = 0.1)
#' # Calculating the ECD utility using baskexact as a backend
#' design_x <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                        p0 = 0.2, backend = "exact")
#' u_ecd(design_x,
#'       x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'       detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                            n = 20,
#'                            weight_fun = baskexact::weights_fujikawa,
#'                            logbase = exp(1)),
#'       penalty = 1, threshold = 0.1)
u_ewp <- function(design, x, detail_params, p1 = NULL,
                  p2 = rep(design$p0, design$k), threshold, penalty,
                  report_details = FALSE,
                  reduce_calculations =
                    ifelse(design$backend == "exact", TRUE, FALSE)) {
  u_result <- NA_real_
  if(!reduce_calculations | design$backend == "sim"){
    if(reduce_calculations){
      message('The argument reduce_calculations = TRUE takes no effect for
              the designs with backend = "sim".')
    }
    details_list <- get_details_for_two_scenarios(design, x, detail_params, p1,
                                                  p2, which_details_list =
                                                    "all")

    ewp <-
      details_list[["p1"]]$EWP
    fwer <-
      details_list[["p2"]]$FWER
    if (fwer >= threshold) {
      u_result <- -fwer*penalty
    } else{
      u_result <- ewp
    }
  } else if(design$backend == "exact"){
    details_list <- list()
    details_list[["p2"]] <- do.call(baskwrap::get_details,
                       c(design = list(design),
                         as.list(x),
                         append_details(set_details(detail_params, "p1", p2),
                                        "which_details", "FWER")))
    fwer <-
      details_list[["p2"]]$FWER
    if (fwer >= threshold) {
      u_result <- -fwer*penalty
    } else{
      detail_params <- io_val_p1(detail_params, p1)
      details_list[["p1"]] <- do.call(baskwrap::get_details,
                         c(design = list(design),
                           as.list(x),
                           append_details(detail_params, "which_details",
                                          "EWP")))
      u_result <- details_list[["p1"]]$EWP
    }
  }
  if(report_details){
    attr(u_result, "details") <- details_list
    attr(u_result, "reduce_calculations") <- reduce_calculations
  }
  return(u_result)
}

#' @rdname u_ewp
#' @export
u_ecd <- function(design, x, detail_params, p1 = NULL,
                  p2 = rep(design$p0, design$k),
                  penalty, threshold,
                  report_details = FALSE,
                  reduce_calculations =
                    ifelse(design$backend == "exact", TRUE, FALSE)) {
  u_result <- NA_real_
  if(!reduce_calculations | design$backend == "sim"){
    if(reduce_calculations){
      message('The argument reduce_calculations = TRUE takes no effect for
              the designs with backend = "sim".')
    }
    details_list <- get_details_for_two_scenarios(design, x, detail_params, p1,
                                                  p2,
                                                  which_details_list =
                                                    "all")
    ecd <-
      details_list[["p1"]]$ECD
    fwer <-
      details_list[["p2"]]$FWER
    if (fwer >= threshold) {
      u_result <- -fwer*penalty
    } else{
      u_result <- ecd
    }
  } else if(design$backend == "exact"){
    details_list <- list()
    details_list[["p2"]] <- do.call(baskwrap::get_details,
                       c(design = list(design),
                         as.list(x),
                         append_details(set_details(detail_params, "p1", p2),
                                        "which_details", "FWER")))
    fwer <-
      details_list[["p2"]]$FWER
    if (fwer >= threshold) {
      u_result <- -fwer*penalty
    } else{
      detail_params <- io_val_p1(detail_params, p1)
      details_list[["p1"]] <- do.call(baskwrap::get_details,
                         c(design = list(design),
                           as.list(x),
                           append_details(detail_params, "which_details",
                                          "ECD")))
      u_result <- details_list[["p1"]]$ECD
    }
  }
  if(report_details){
    attr(u_result, "details") <- details_list
    attr(u_result, "reduce_calculations") <- reduce_calculations
  }
  return(u_result)
}


#' Utility functions: Two-level power-error combination functions
#'
#' These utility functions combine rewarding power and penalizing TOER by
#' subtracting TOER from power. In addition, unacceptably high FWER above a
#' certain threshold receives harsher penalty.
#'
#' The utility function \eqn{u_{\text{2ewp}}} is defined as
#' \deqn{u_{\text{2ewp}}(\boldsymbol\phi,\mathbf{p}) = \mathrm{ewp}(\boldsymbol
#' \phi,\mathbf{p}) - \left(\xi_1\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) +
#' \xi_2(\mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta)\mathbf 1(
#' \mathrm{fwer}(\boldsymbol\phi,\mathbf{p}) - \eta) \right),}
#' where \eqn{\eta\in[0,1]} is a threshold for imposing harsher FWER penalty.
#'
#' The utility function \eqn{u_{\text{2pow}}} is defined analogously as
#' \deqn{u_{\text{2pow}}(\boldsymbol\phi,\mathbf{p}) = \sum_{i\in R}\mathrm{pow}_i(\boldsymbol
#' \phi,\mathbf{p}) - \sum_{j\in R^c}\left(\xi_1\mathrm{toer}_j(\boldsymbol\phi,\mathbf{p}) +
#' \xi_2(\mathrm{toer}_j(\boldsymbol\phi,\mathbf{p}) - \eta)\mathbf 1(
#' \mathrm{toer}_j(\boldsymbol\phi,\mathbf{p}) - \eta) \right),}
#' where \eqn{R} and \eqn{R^c} are the sets of active and inactive strata, respectively.
#'
#' @inheritParams params_main
#' @param p1 A numeric, response scenario for calculating power and error rate.
#' @param penalty1 A numeric, `penalty1` is the penalty for low FWER,
#' @param penalty2 A numeric, `penalty1 + penalty2` is the penalty for high
#' FWER.
#' @param threshold A numeric, for high FWER above this threshold we impose a
#' harsher penalty.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' u_2ewp(design,
#'        x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'        detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                             n = 20,
#'                             weight_fun = baskexact::weights_fujikawa,
#'                             logbase = exp(1)),
#'        penalty1 = 1, penalty2 = 2,
#'        threshold = 0.1)
#' u_2pow(design,
#'        x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'        detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                             n = 20,
#'                             weight_fun = baskexact::weights_fujikawa,
#'                             logbase = exp(1)),
#'        penalty1 = 1, penalty2 = 2,
#'        threshold = 0.1)
u_2ewp <- function(design, x, detail_params, p1 = NULL,
                   penalty1, penalty2, threshold,
                   report_details = FALSE) {
  detail_params$which_details <- c(detail_params$which_details, "FWER", "EWP")
  detail_params <- io_val_p1(detail_params, p1)
  details <- do.call(baskwrap::get_details,
                     c(design = list(design), as.list(x), detail_params))
  u_result <- NA_real_
  ewp <-
    details$EWP
  fwer <- details$FWER
  if(fwer > threshold){
    u_result <- ewp - (penalty1*fwer + penalty2*(fwer - threshold))
  } else{
    u_result <- ewp - (penalty1*fwer)
  }
  if(report_details){
    attr(u_result, "details") <- list(p1 = details)
  }
  return(u_result)
}

#' @rdname u_2ewp
#' @export
u_2pow <- function(design, x, detail_params, p1 = NULL,
                   penalty1, penalty2, threshold,
                   report_details = FALSE) {
  detail_params$which_details <- c(detail_params$which_details,
                                   "Rejection_Probabilities")
  detail_params <- io_val_p1(detail_params, p1)
  details <- do.call(baskwrap::get_details,
                     c(design = list(design), as.list(x), detail_params))
  alternative <- design$p0 != detail_params$p1
  u_result <- sum(details$Rejection_Probabilities[alternative]) -
    penalty1*sum(details$Rejection_Probabilities[!alternative]) -
    penalty2*
      sum(details$Rejection_Probabilities[!alternative &
                                          (details$Rejection_Probabilities >
                                             threshold)] -
            threshold)
  if(report_details){
    attr(u_result, "details") <- list(p1 = details)
  }
  return(u_result)
}

#' Utility function: Scenario-averaged utility function
#'
#' For a utility function \eqn{u(\cdot, \mathbf p)} and a set of true scenarios
#' \eqn{\{\mathbf p_i,\ldots\}}, calculate the weighted average utility function
#' \deqn{ \bar u(x) = \sum_i w_i u(x, \mathbf p_i)}
#' for a set of weights with \eqn{\sum_i w_i=1}. By default,
#' \eqn{w_i=\frac{1}{|\{\mathbf p_i,\ldots\}|}} for all \eqn{i}.
#'
#' @inheritParams params_main
#' @param detail_params A named list of parameters that need to be supplied to
#' `get_details()`. It must not contain `p1`,
#' as this is supplied separately.
#' @inheritParams params_utility_caller
#' @param p1s A numeric array in which each row defines a scenario of true
#' response rates under the alternative hypothesis.
#' @param weights_u A numeric vector of weights for calculating the weighted
#' average.
#' @param penalty_maxtoer A numeric, the penalty for punishing the maximal TOER across
#' all considered scenarios and all strata.
#' @param threshold_maxtoer A numeric, above this threshold maximal TOER is punished
#' by returning `-penalty_maxtoer` times the maximal TOER. Default is `NULL`, which
#' means no penalty.
#' @param use_future A logical, should `future_apply()` instead of `apply()`
#' be used for calculating the utilities to be averaged over. You must still
#' activate a *future* backend for this option to take any effect, default is
#' `FALSE`.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
#' detail_params <- list(n = 20,
#'                       weight_fun = baskexact::weights_fujikawa,
#'                       logbase = exp(1))
#' p1s <- rbind(c(0.2,0.2,0.2), c(0.2,0.2,0.5), c(0.2,0.5,0.5), c(0.5,0.5,0.5))
#' # Averaging over u_ewp()
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = list(penalty = 1, threshold = 0.1),
#'       p1s = p1s
#'       )
#' # Averaging over u_2ewp()
#' utility_params_2ewp <- list(penalty1 = 1, penalty2 = 2, threshold = 0.1)
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_2ewp,
#'       utility_params = utility_params_2ewp,
#'       p1s = p1s
#'       )
#' # Punishing maximal TOER in all scenarios and all strata
#' u_avg(design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_2ewp,
#'       utility_params = utility_params_2ewp,
#'       p1s = p1s,
#'       penalty_maxtoer = 1, threshold_maxtoer = 0.1
#'       )
u_avg <- function(design, x, detail_params, utility, utility_params,
                  p1s, weights_u = rep(1/nrow(p1s), nrow(p1s)),
                  report_details = FALSE,
                  penalty_maxtoer = NULL, threshold_maxtoer = NULL,
                  use_future = FALSE){
  if(xor(is.null(penalty_maxtoer), is.null(threshold_maxtoer))){
    stop("Only one of penalty_maxtoer and threshold_maxtoer was supplied. You must supply both or nothing at all.")
  }
  detail_params$which_details <- c(detail_params$which_details,
                                   "Rejection_Probabilities")
  u_result <- NA_real_
  utility_params["report_details"] <- (report_details |
                                         !is.null(threshold_maxtoer))
  u_fun <- function(p1){
    detail_params_sc <- detail_params
    detail_params$p1 <- p1
    do.call(utility, c(design = list(design),
                       x = list(x),
                       detail_params = list(detail_params),
                       utility_params))}
  # Calculate utility for every scenario in the p1s
  if(!use_future){
    # message("Use of future_apply in u_avg() is switched off.")
    u_vals <- apply(X = p1s, MARGIN = 1, FUN = u_fun, simplify = FALSE)
  } else{
    # message("Use of future_apply in u_avg() is switched on.")
    u_vals <- future.apply::future_apply(X = p1s, MARGIN = 1, FUN = u_fun,
                                         simplify = FALSE, future.seed = TRUE)
  }
  # Resulting utility is the weighted mean of utilities
  u_result <- sum(as.numeric(u_vals)*weights_u)
  # Punish maximal TOER rate in strata if requested, maximum is formed
  # across all scenarios and all strata
  if(!is.null(threshold_maxtoer)){
    if(!is.null(attr(u_vals[[1]], "reduce_calculations"))){
      if(attr(u_vals[[1]], "reduce_calculations")){
        stop(
          "It appears that you are trying to calculate maximal stratum-wise rejection
  probabilities for u_ewp() or u_avg() with the option
  utility_params$reduce_calculations == TRUE.
  This may yield incorrect results. Please switch the utility function's
  parameter to
  utility_params$reduce_calculations <- FALSE.")
      }
    }
    toers <- lapply(u_vals,
           function(x){
             names_det <- names(attr(x, "details"))
             return(lapply(names_det,
                           function(y) {
                             details <- attr(x, "details")[[y]]
                             return(details[["Rejection_Probabilities"]][
                               which(details[["p0"]] == details[["p1"]])
                                     ]
                             )
                           }
                          )
                    )
    })
    toer_max <- max(unlist(toers))
    if(toer_max >= threshold_maxtoer){
      u_result <-  -penalty_maxtoer*toer_max
    }
  }
  # Report details for each response scenario of the p1s
  if(report_details){
    details_list <- lapply(u_vals, function(x) attr(x, "details"))
    names(details_list) <- paste0("c(",
                                  apply(p1s,
                                        1,
                                        function(x) paste(x, collapse = ", ")),
                                  ")")
    attr(u_result, "details") <- details_list
  }
  return(u_result)
}

#' Utility function with boundaries on the parameters
#'
#' This function manually implements boundaries for a given utility function
#' `utility`. If the vector `x` lies out of the lower and upper bounds, the
#' function returns `NA_real_`. Else it returns the utility functions value.
#'
#' @inheritParams params_main
#' @inheritParams params_utility_caller
#' @param lower numerical, a vector of lower bounds of the parameters.
#' @param upper numerical, a vector of upper bounds of the parameters.
#'
#' @inherit u_ewp return
#' @export
#'
#' @examples
#' design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
#'                                      p0 = 0.2, backend = "exact")
#' lower <- list(lambda = 0, epsilon = 1, tau = 0)
#' upper <- list(lambda = 1, epsilon = 10, tau = 1)
#' utility_params <- list(penalty = 1, threshold = 0.1)
#' detail_params <- list(p1 = c(0.5, 0.2, 0.2),
#'                       n = 20,
#'                       weight_fun = baskexact::weights_fujikawa,
#'                       logbase = exp(1))
#' # Out of bounds
#' u_bnd(design = design,
#'       x = list(lambda = 1.3, epsilon = 2, tau = 0.5),
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = utility_params,
#'       lower = lower,
#'       upper = upper)
#' # Inside bounds, this is the same as u_ewp
#' x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
#' u_bnd(design = design,
#'       x = x,
#'       detail_params = detail_params,
#'       utility = u_ewp,
#'       utility_params = utility_params,
#'       lower = lower,
#'       upper = upper)
#' u_ewp(design = design,
#'       x = x,
#'       detail_params = detail_params, penalty = 1, threshold = 0.1)
u_bnd <-
  function(design, x, detail_params, utility, utility_params,
           lower, upper,
           report_details = FALSE) {
    if (!all(as.numeric(lower) <= as.numeric(x)) |
        !(all(as.numeric(x) <= as.numeric(upper)))) {
      return(NA_real_)
    } else{
      return(do.call(utility, c(list(design = design,
                                     x = x,
                                     detail_params = detail_params),
                                utility_params,
                                report_details = FALSE)))
    }
  }


