test_that("grid search on small grid with u_2pow() works", {
  axes <- list(lambda = c(0.1, 0.99),
               epsilon = c(1, 2),
               tau = c(0.01, 0.5))
  threshold <- 0.1
  penalty1 <- 1
  penalty2 <- 2
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
                                                 control = list(fnscale = -1,
                                                                use_future = FALSE)),
                         x_names = c("lambda", "epsilon", "tau"),
                         trace = TRUE,
                         progress_bar = 8 + 2)
  # Low lambda, low tau and low epsilon imply higher type-I error rates,
  # hence we expect that c(lambda = 0.99, epsilon = 2, tau = 0.5) is the optimal
  # parameter vector.
  opt_ref <- c(lambda = 0.99, epsilon = 2, tau = 0.5)
  details <- baskwrap::get_details(design4,
                                   n = detail_params_fuj$n,
                                   p1 = p1_high,
                                   lambda = opt_ref["lambda"],
                                   epsilon = opt_ref["epsilon"],
                                   tau = opt_ref["tau"],
                                   logbase = logbase)
  expect_equal(res$par, opt_ref)
  expect_equal(res$value, details$Rejection_Probabilities[3] +
                 details$Rejection_Probabilities[4] +
                 - penalty1*(details$Rejection_Probabilities[1] +
                               details$Rejection_Probabilities[2])
                 - penalty2*(details$Rejection_Probabilities[1] +
                               details$Rejection_Probabilities[2] -
                               2*threshold))
})
