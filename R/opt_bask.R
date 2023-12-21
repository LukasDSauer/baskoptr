# README: Here, I combined elements from different projects that I want to combine:
# Lukas Baumanns's opt_design function and the skeleton of an S3 function that I
# began to write for baskexact.
#' @describeIn opt_design Optimize a single-stage basket design.
#'
#' @template design
#' @template n
#' @template alpha
#' @template weights
#' @template globalweights
#' @template scenarios
#' @template prec_digits
#' @template dotdotdot
setMethod("opt_design", "OneStageBasket",
          function(design, n, alpha, weight_fun, weight_params = list(),
                   globalweight_fun = NULL, globalweight_params = list(),
                   scenarios, prec_digits, ...) {
            all_params <- c(weight_params, globalweight_params)
            grid <- expand.grid(all_params)
            if (length(all_params) == 0) {
              lgrid <- 1
            } else {
              lgrid <- nrow(grid)
            }

            l1 <- length(weight_params)
            l2 <- length(globalweight_params)
            lambdas <- numeric(lgrid)
            p <- progressr::progressor(steps = lgrid)

            ecd_res <- foreach(i = 1:lgrid, .combine = 'rbind') %dofuture% {
              res_loop <- numeric(ncol(scenarios) + 1)
              if (l1 >= 1) {
                ploop1 <- as.list(grid[i, 1:l1, drop = FALSE])
              } else {
                ploop1 <- list()
              }
              if (l2 >= 1) {
                ploop2 <- as.list(grid[i, (l1 + 1):(l1 + l2), drop = FALSE])
              } else {
                ploop2 <- list()
              }

              l <- do.call(adjust_lambda, args = c(design = list(design), n = n,
                                                   p1 = NULL, alpha = alpha, weight_fun = weight_fun,
                                                   weight_params = list(ploop1), globalweight_fun = globalweight_fun,
                                                   globalweight_params = list(ploop2), prec_digits = prec_digits, ...))
              res_loop[1] <- l$lambda

              for (j in 1:ncol(scenarios)) {
                res_loop[j + 1] <- do.call(ecd, args = c(design = list(design),
                                                         p1 = list(scenarios[, j]), n = n, lambda = l$lambda,
                                                         weight_fun = weight_fun, weight_params = list(ploop1),
                                                         globalweight_fun = globalweight_fun,
                                                         globalweight_params = list(ploop2), ...))
              }
              p()
              res_loop
            }
            if (lgrid == 1) {
              names(ecd_res) <- c("Lambda", colnames(scenarios))
              c(ecd_res, "Mean ECD" = mean(ecd_res[-1]))
            } else {
              colnames(ecd_res) <- c("Lambda", colnames(scenarios))
              ecd_res <- cbind(grid, ecd_res, "Mean ECD" = rowMeans(ecd_res[, -1]))
              ecd_res <- ecd_res[order(ecd_res[, ncol(ecd_res)], decreasing = TRUE), ]
              rownames(ecd_res) <- NULL
              ecd_res
            }
          })



#' Optimize a Basket Trial Design
#'
#' @template design
#' @template n
#' @template alpha
#' @template design_params
#' @param scenarios A matrix of scenarios.
#' @template prec_digits
#' @template iter
#' @param data A list of data matrices generated with \code{get_data}. The
#'   list elements have to correspond to the columsn of \code{scenarios}.
#' @template dotdotdot
#'
#' @return A matrix with the expected number of correct decisions.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' scenarios <- get_scenarios(design, p1 = 0.5)
#'
#' # Without simulated data
#' opt_design(design, n = 20, alpha = 0.05, design_params =
#'   list(epsilon = c(1, 2), tau = c(0, 0.5)), scenarios = scenarios,
#'   prec_digits = 3)
#'
#' # With simulated data
#' scenario_list <- as.list(data.frame(scenarios))
#' data_list <- lapply(scenario_list,
#'   function(x) get_data(k = 3, n = 20, p = x, iter = 1000))
#' opt_design(design, n = 20, alpha = 0.05, design_params =
#'   list(epsilon = c(1, 2), tau = c(0, 0.5)), scenarios = scenarios,
#'   prec_digits = 3, data = data_list)
opt_design <- function(design, n, alpha, design_params = list(), scenarios,
                       prec_digits, iter = 1000, data = NULL, ...) {
  check_data_list(data = data, scenarios = scenarios)
  check_scenarios(scenarios = scenarios, design = design)
  grid <- expand.grid(design_params)
  if (length(design_params) == 0) {
    lgrid <- 1
  } else {
    lgrid <- nrow(grid)
  }
  p <- progressr::progressor(steps = lgrid)

  ecd_res <- matrix(nrow = lgrid, ncol = ncol(scenarios))
  colnames(ecd_res) <- colnames(scenarios)
  lambdas <- numeric(lgrid)
  null_scen <- which(apply(scenarios, 2, function(x) all(x == design$p0)))

  for (i in 1:lgrid) {
    params_loop <- lapply(as.list(grid), function(x) x[i])
    l <- do.call(adjust_lambda, args = list(design = design, n = n,
                                            p1 = NULL, alpha = alpha, design_params = params_loop, iter = iter,
                                            prec_digits = prec_digits, data = data[[null_scen]], ...))
    lambdas[i] <- l$lambda

    for (j in 1:ncol(scenarios)) {
      ecd_res[i, j] <- do.call(ecd, args = c(design = list(design), n = n,
                                             p1 = list(scenarios[, j]), lambda = l$lambda, params_loop,
                                             iter = iter, data = list(data[[j]]), ...))
    }
    p()
  }

  if (ncol(grid) == 0) {
    ecd_res <- cbind("Lambda" = lambdas, ecd_res,
                     "Mean_ECD" = rowMeans(ecd_res))
  } else {
    ecd_res <- cbind(grid, "Lambda" = lambdas, ecd_res,
                     "Mean_ECD" = rowMeans(ecd_res))
    ecd_res <- ecd_res[order(ecd_res[, ncol(ecd_res)], decreasing = TRUE), ]
  }
  ecd_res
}

#' Optimize a Basket Trial Design
#'
#' Optimize the parameters of a basket trial design using a utility-based
#' approach with a simulation algorithm of your choice.
#'
#' @template design
#' @template utility
#' @template algorithm
#' @template detail_params
#' @template utility_params
#' @template algorithm_params
#' @template trace
#'
#' @inherit optimization_optim_sa return
#'
#' @export
#'
#' @examples
#' design <- setupOneStageBasket(k, shape1 = 1, shape2 = 1, p0 = 0.2)
#' opt_design_gen(design = design,
#'                utility = u_ewp_discont,
#'                algorithm = optimizr::simann,
#'                detail_params = list(p1 = c(0.5, 0.2, 0.2),
#'                                    n = 20,
#'                                    weight_fun = weights_fujikawa,
#'                                    logbase = exp(1)),
#'                utility_params = list(thresh = 0.05),
#'                algorithm_params = list(par = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        lower = c(lambda = 0.001,
#'                                                     epsilon = 1,
#'                                                     tau = 0.001),
#'                                        upper = c(lambda = 0.999,
#'                                                  epsilon = 10,
#'                                                  tau = 0.999),
#'                                        control = list(maxit = 10000,
#'                                                       temp = 2000)))
#' opt_design_gen(design = design,
#'                utility = u_powfwer_discont_bound,
#'                algorithm = stats_optim_sann,
#'                detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                     logbase = exp(1), exact = TRUE),
#'                utility_params = list(alpha = 0.05,
#'                                      lower = c(lambda = 0.001,
#'                                                epsilon = 1,
#'                                                tau = 0.001),
#'                                       upper = c(lambda = 0.999,
#'                                                 epsilon = 10,
#'                                                 tau = 0.999)),
#'                algorithm_params = list(start = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        maximization = TRUE,
#'                                        control = list(maxit = 30000,
#'                                                       temp = 2000,
#'                                                       REPORT = 2)))
opt_design_gen <- function(design, utility, algorithm, detail_params,
                           utility_params, algorithm_params, trace = TRUE){
  x_names <- character()
  if(!is.null(algorithm_params$lower)){
    x_names <- names(algorithm_params$lower)
  } else if(!is.null(algorithm_params$par)){
    x_names <- names(algorithm_params$par)
  } else {
    stop("Cannot retrieve parameter vector names from algorithm_params. Please
         supply a 'lower' or a 'par'  argument in algorithm_params.")
  }
  u_fun <- function(x){
    x_named <- x
    names(x_named) <- x_names
    do.call(utility, c(design = list(design),
                       x = list(x_named),
                       detail_params = list(detail_params),
                       utility_params))}
  res <- do.call(algorithm,
                 c(fun = u_fun,
                   trace = trace,
                   algorithm_params))
}
