# Parameters
threshold <- 0.1
penalty1 <- 1
penalty2 <- 2
alg_control <- list(fnscale = -1,
                    use_future = FALSE,
                    REPORT = NA_integer_)
# Reference values
#   Low lambda, low tau and low epsilon imply higher type-I error rates,
#   hence we expect that c(lambda = 0.99, epsilon = 2, tau = 0.5) is the optimal
#   parameter vector.
opt_ref <- c(lambda = 0.99, epsilon = 2, tau = 0.5)
details <- baskwrap::get_details(design4,
                                 n = detail_params_fuj$n,
                                 p1 = p1_high,
                                 lambda = opt_ref["lambda"],
                                 epsilon = opt_ref["epsilon"],
                                 tau = opt_ref["tau"],
                                 logbase = logbase)

val_ref <- details$Rejection_Probabilities[3] +
  details$Rejection_Probabilities[4] -
  penalty1*(details$Rejection_Probabilities[1] +
              details$Rejection_Probabilities[2])-
  penalty2*(details$Rejection_Probabilities[1] +
              details$Rejection_Probabilities[2] -
              2*threshold)
format_fun <- function(res) {
  return(c(res[["par"]], value = res[["value"]]))
}

test_that("grid search on small grid with u_2pow() works", {
  axes <- list(lambda = c(0.1, 0.99),
               epsilon = c(1, 2),
               tau = c(0.01, 0.5))
  res <- opt_design_gen(design = design4,
                         utility = u_2pow,
                         algorithm = optimizr::gridsearch,
                         detail_params = NULL,
                         utility_params = list(detail_params = detail_params_fuj,
                                               p1 = p1_high,
                                               threshold = threshold,
                                               penalty1 = penalty1,
                                               penalty2 = penalty2),
                         algorithm_params = list(axes = axes,
                                                 control = alg_control),
                         x_names = c("lambda", "epsilon", "tau"),
                         trace = FALSE)
  expect_equal(res$par, opt_ref)
  expect_equal(res$value, val_ref)
  expect_true(is.null(res$trace))
})

test_that("trace options work", {
  set.seed(1718)
  grid <- data.frame(lambda = c(0.9, 0.9), epsilon = c(1, 2), tau = c(0.5, 0.5))
  # Not returning trace.
  res <- opt_design_gen(design = design4,
                        utility = u_2pow,
                        algorithm = optimizr::gridsearch,
                        detail_params = NULL,
                        utility_params = list(detail_params = detail_params_fuj,
                                              p1 = p1_high,
                                              threshold = threshold,
                                              penalty1 = penalty1,
                                              penalty2 = penalty2),
                        algorithm_params = list(grid = grid,
                                                control = alg_control),
                        x_names = c("lambda", "epsilon", "tau"),
                        trace = "")
  expect_true(is.null(res$trace))
  res <- opt_design_gen(design = design4,
                        utility = u_2pow,
                        algorithm = optimizr::gridsearch,
                        detail_params = NULL,
                        utility_params = list(detail_params = detail_params_fuj,
                                              p1 = p1_high,
                                              threshold = threshold,
                                              penalty1 = penalty1,
                                              penalty2 = penalty2),
                        algorithm_params = list(grid = grid,
                                                control = alg_control),
                        x_names = c("lambda", "epsilon", "tau"),
                        trace = NULL)
  expect_true(is.null(res$trace))
  # Returning trace.
  res <- opt_design_gen(design = design4,
                        utility = u_2pow,
                        algorithm = optimizr::gridsearch,
                        detail_params = NULL,
                        utility_params = list(detail_params = detail_params_fuj,
                                              p1 = p1_high,
                                              threshold = threshold,
                                              penalty1 = penalty1,
                                              penalty2 = penalty2),
                        algorithm_params = list(grid = grid,
                                                control = alg_control),
                        x_names = c("lambda", "epsilon", "tau"),
                        trace = "return")
  expect_equal(res$trace$lambda, grid$lambda)
  expect_equal(res$trace$epsilon, grid$epsilon)
  # Saving trace to custom path.
  trace_path <- test_path(path_refdata_rel, "ref_trace.RDS")
  expect_false(file.exists(trace_path))
  res <- opt_design_gen(design = design4,
                        utility = u_2pow,
                        algorithm = optimizr::gridsearch,
                        detail_params = NULL,
                        utility_params = list(detail_params = detail_params_fuj,
                                              p1 = p1_high,
                                              threshold = threshold,
                                              penalty1 = penalty1,
                                              penalty2 = penalty2),
                        algorithm_params = list(grid = grid,
                                                control = alg_control),
                        x_names = c("lambda", "epsilon", "tau"),
                        trace = trace_path)
  expect_true(file.exists(trace_path))
  expect_true(res$value %in% res$trace$fn)
  file.remove(trace_path)
  # Wrong trace options, e.g. numbers and wrong strings
  expect_error({opt_design_gen(design = design4,
                              utility = u_2pow,
                              algorithm = optimizr::gridsearch,
                              detail_params = NULL,
                              utility_params = list(detail_params = detail_params_fuj,
                                                    p1 = p1_high,
                                                    threshold = threshold,
                                                    penalty1 = penalty1,
                                                    penalty2 = penalty2),
                              algorithm_params = list(grid = grid,
                                                      control = alg_control),
                              x_names = c("lambda", "epsilon", "tau"),
                              trace = "wrongfile.txt")})
  expect_error({opt_design_gen(design = design4,
                               utility = u_2pow,
                               algorithm = optimizr::gridsearch,
                               detail_params = NULL,
                               utility_params = list(detail_params = detail_params_fuj,
                                                     p1 = p1_high,
                                                     threshold = threshold,
                                                     penalty1 = penalty1,
                                                     penalty2 = penalty2),
                               algorithm_params = list(grid = grid,
                                                       control = alg_control),
                               x_names = c("lambda", "epsilon", "tau"),
                               trace = 123)})
})

test_that("grid search can retrieve parameter names, trace can be switched
           off, output can be manually formatted", {

  res <- opt_design_gen(design = design4,
                        utility = u_2pow,
                        algorithm = optimizr::gridsearch,
                        detail_params = NULL,
                        utility_params = list(detail_params = detail_params_fuj,
                                              p1 = p1_high,
                                              threshold = threshold,
                                              penalty1 = penalty1,
                                              penalty2 = penalty2),
                        algorithm_params = list(lower = c(lambda = 0.1,
                                                          epsilon = 1,
                                                          tau = 0.01),
                                                upper = c(lambda = 0.99,
                                                          epsilon = 2,
                                                          tau = 0.5),
                                                step = c(lambda = 0.89,
                                                         epsilon = 1,
                                                         tau = 0.49),
                                                control = alg_control),
                        trace = "none",
                        format_result = format_fun)
  expect_equal(res, c(opt_ref, value = val_ref), ignore_attr = T)
})

test_that("if trace is switched on but format_fun formats without res$par,
a warning is dropped and trace is switched off.", {
  expect_warning({res <- opt_design_gen(design = design4,
                       utility = u_2pow,
                       algorithm = optimizr::gridsearch,
                       detail_params = NULL,
                       utility_params = list(detail_params = detail_params_fuj,
                                             p1 = p1_high,
                                             threshold = threshold,
                                             penalty1 = penalty1,
                                             penalty2 = penalty2),
                       algorithm_params = list(lower = c(lambda = 0.1,
                                                         epsilon = 1,
                                                         tau = 0.01),
                                               upper = c(lambda = 0.99,
                                                         epsilon = 2,
                                                         tau = 0.5),
                                               step = c(lambda = 0.89,
                                                        epsilon = 1,
                                                        tau = 0.49),
                                               control = alg_control),
                       trace = TRUE,
                       format_result = format_fun)})
  expect_equal(res, c(opt_ref, value = val_ref), ignore_attr = T)
})

test_that("algorithms with wrong format do not return final details or trace,
but return the correct result", {
             expect_warning({res_nofin <- opt_design_gen(design = design4,
                                   utility = u_2pow,
                                   algorithm = optimizr::gridsearch,
                                   detail_params = NULL,
                                   utility_params = list(detail_params = detail_params_fuj,
                                                         p1 = p1_high,
                                                         threshold = threshold,
                                                         penalty1 = penalty1,
                                                         penalty2 = penalty2),
                                   algorithm_params = list(lower = c(lambda = 0.1,
                                                                     epsilon = 1,
                                                                     tau = 0.01),
                                                           upper = c(lambda = 0.99,
                                                                     epsilon = 2,
                                                                     tau = 0.5),
                                                           step = c(lambda = 0.89,
                                                                    epsilon = 1,
                                                                    tau = 0.49),
                                                           control = alg_control),
                                   trace = "none",
                                   format_result = format_fun,
                                   final_details = TRUE)})
             expect_warning({res_notra <- opt_design_gen(design = design4,
                                                         utility = u_2pow,
                                                         algorithm = optimizr::gridsearch,
                                                         detail_params = NULL,
                                                         utility_params = list(detail_params = detail_params_fuj,
                                                                               p1 = p1_high,
                                                                               threshold = threshold,
                                                                               penalty1 = penalty1,
                                                                               penalty2 = penalty2),
                                                         algorithm_params = list(lower = c(lambda = 0.1,
                                                                                           epsilon = 1,
                                                                                           tau = 0.01),
                                                                                 upper = c(lambda = 0.99,
                                                                                           epsilon = 2,
                                                                                           tau = 0.5),
                                                                                 step = c(lambda = 0.89,
                                                                                          epsilon = 1,
                                                                                          tau = 0.49),
                                                                                 control = alg_control),
                                                         trace = "return",
                                                         format_result = format_fun,
                                                         final_details = FALSE)})
             expect_equal(res_nofin, c(opt_ref, value = val_ref), ignore_attr = T)
           })

test_that("simulated annealing can retrieve parameter names, trace recorded by
          algorithm is saved in trace_alg if trace is also recorded by
          opt_design_gen", {
  set.seed(1213)
  p0 <- 0.2
  p1 <- c(0.2, 0.4, 0.4)
  threshold <- 0.15
  penalty <- 2
  design3 <- baskwrap::setup_fujikawa_x(k = 3,
                                        shape1 = 1,
                                        shape2 = 1,
                                        p0 = p0,
                                        backend = "exact")
  utility_params <- list(p1 = p1,
                         threshold = threshold,
                         penalty = penalty)
  res <- progressr::with_progress({
    opt_design_gen(design = design3,
                   utility = u_ewp,
                   algorithm = optimizr::simann,
                   detail_params = detail_params_fuj,
                   utility_params = utility_params,
                   algorithm_params = list(par = c(lambda = 0.9,
                                                   epsilon = 1,
                                                   tau = 0.5),
                                           control = list(maxit = 10,
                                                          temp = 1,
                                                          parscale = 0.01,
                                                          fnscale = -1,
                                                          REPORT = 1)),
                   trace = TRUE,
                   progress_bar = 10 + 2,
                   final_details = TRUE,
                   final_details_utility_params = c(utility_params,
                                                    reduce_calculations =
                                                      FALSE)
                   )})
  details_p0 <- baskwrap::get_details(design3,
                                   n = detail_params_fuj$n,
                                   p1 = rep(p0, design3$k),
                                   lambda = res$par["lambda"],
                                   epsilon = res$par["epsilon"],
                                   tau = res$par["tau"],
                                   logbase = logbase,
                                   verbose = F)
  details_p1 <- baskwrap::get_details(design3,
                                   n = detail_params_fuj$n,
                                   p1 = p1,
                                   lambda = res$par["lambda"],
                                   epsilon = res$par["epsilon"],
                                   tau = res$par["tau"],
                                   logbase = logbase,
                                   verbose = F)
  if(details_p0$FWER < threshold){
    expect_equal(details_p1$EWP, res$value)
  } else {
    expect_equal(-details_p0$FWER*penalty, res$value)
  }
  expect_equal(attr(res, "final_details")$p2$Rejection_Probabilities,
               details_p0$Rejection_Probabilities)
  expect_equal(attr(res, "final_details")$p1$Rejection_Probabilities,
               details_p1$Rejection_Probabilities)
  expect_equal(nrow(res$trace), 12)
  expect_equal(nrow(res$trace_alg), 10)
  expect_true(all(res$value >= res$trace$fn))
  expect_equal(length(which(res$value > res$trace$fn)), nrow(res$trace) - 2)
})


test_that("error message for duplicate detail_params is thrown",
          {
  expect_error(opt_design_gen(design = design4,
                              utility = u_2pow,
                              algorithm = optimizr::gridsearch,
                              # First detail_params
                              detail_params = detail_params_fuj,
                              # Second detail_params
                              utility_params = list(detail_params = detail_params_fuj,
                                                    p1 = p1_high,
                                                    threshold = threshold,
                                                    penalty1 = penalty1,
                                                    penalty2 = penalty2),
                              algorithm_params = list(lower = c(lambda = 0.1,
                                                                epsilon = 1,
                                                                tau = 0.01),
                                                      upper = c(lambda = 0.99,
                                                                epsilon = 2,
                                                                tau = 0.5),
                                                      step = c(lambda = 0.89,
                                                               epsilon = 1,
                                                               tau = 0.49),
                                                      control = alg_control),
                              trace = "",
                              format_result = format_fun,
                              final_details = TRUE))
          })

test_that("error message for lack of parameter names is thrown",
          {
  expect_error(opt_design_gen(design = design4,
                              utility = u_2pow,
                              algorithm = optimizr::gridsearch,
                              detail_params = detail_params_fuj,
                              utility_params = list(detail_params = detail_params_fuj,
                                                    p1 = p1_high,
                                                    threshold = threshold,
                                                    penalty1 = penalty1,
                                                    penalty2 = penalty2),
                              algorithm_params = list(grid = data.frame(
                                lambda = 0.5,
                                epsilon = 1,
                                tau = 0.5
                              )),
                              trace = FALSE,
                              format_result = format_fun,
                              final_details = TRUE))
          })

