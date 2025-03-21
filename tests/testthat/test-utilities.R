details_conservative <- baskwrap::get_details(design4,
                                              n = detail_params_fuj$n,
                                              p1 = p1_high,
                                              lambda = lambda_conservative,
                                              epsilon = epsilon,
                                              tau = tau,
                                              logbase = logbase)
details_conservative_lowpower <- baskwrap::get_details(design4,
                                              n = detail_params_fuj$n,
                                              p1 = p1_low,
                                              lambda = lambda_conservative,
                                              epsilon = epsilon,
                                              tau = tau,
                                              logbase = logbase)
details_liberal <- baskwrap::get_details(design4,
                                         n = detail_params_fuj$n,
                                         p1 = p0,
                                         lambda = lambda_liberal,
                                         epsilon = epsilon,
                                         tau = tau,
                                         logbase = logbase)

test_that("u_avg() returns an error message if used with  u_ewp(),
          reduce_calculations == FALSE and 'exact' backend.", {
  design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
                                       p0 = 0.2, backend = "exact")
  x <- list(lambda = 0.99, epsilon = 2, tau = 0.5)
  detail_params <- list(n = 20,
                        weight_fun = baskexact::weights_fujikawa,
                        logbase = exp(1))
  expect_error(u_avg(design,
                     x = x,
                     detail_params = detail_params,
                     utility = u_ewp,
                     utility_params = list(penalty = 1, threshold = 0.1),
                     p1s = p1s,
                     penalty_maxtoer = 1, threshold_maxtoer = 0.1
                     ))
})
test_that("u_ewp() delivers the expected results", {
  penalty <- 3
  u_conservative <- u_ewp(design = design4,
              x = x_fuj_conservative,
              detail_params = detail_params_fuj,
              p1 = p1_high,
              threshold = 0.1,
              penalty = penalty,
              report_details = TRUE)
  u_liberal <- u_ewp(design = design4,
                          x = x_fuj_liberal,
                          detail_params = detail_params_fuj,
                          p1 = p1_high,
                          threshold = 0.1,
                          penalty = penalty,
                          report_details = TRUE)
  expect_equal(u_conservative, details_conservative$EWP, ignore_attr = T)
  expect_equal(u_liberal, -penalty*details_liberal$FWER, ignore_attr = T)
  expect_true(u_conservative > 0)
  expect_true(u_liberal < 0)
})

test_that("u_ecd() delivers the expected results", {
  penalty <- 2
  u_conservative <- u_ecd(design = design4,
                          x = x_fuj_conservative,
                          detail_params = detail_params_fuj,
                          p1 = p1_low,
                          threshold = 0.1,
                          penalty = penalty,
                          report_details = TRUE)
  u_liberal <- u_ecd(design = design4,
                     x = x_fuj_liberal,
                     detail_params = detail_params_fuj,
                     p1 = p1_high,
                     threshold = 0.1,
                     penalty = penalty,
                     report_details = TRUE)
  expect_equal(u_conservative, details_conservative_lowpower$ECD, ignore_attr = T)
  expect_equal(u_liberal, -penalty*details_liberal$FWER, ignore_attr = T)
  expect_true(u_conservative > 0)
  expect_true(u_liberal < 0)
})

test_that("u_2ewp() delivers the expected results", {
  penalty1 <- 1
  penalty2 <- 2
  threshold <- 0.2
  u_conservative <- u_2ewp(design = design4,
                          x = x_fuj_conservative,
                          detail_params = detail_params_fuj,
                          p1 = p1_high,
                          threshold = threshold,
                          penalty1 = penalty1,
                          penalty2 = penalty2,
                          report_details = TRUE)
  u_liberal <- u_2ewp(design = design4,
                     x = x_fuj_liberal,
                     detail_params = detail_params_fuj,
                     p1 = p1_high,
                     threshold = threshold,
                     penalty = penalty,
                     report_details = TRUE)
  expect_equal(u_conservative,
               details_conservative$EWP - penalty1*details_conservative$FWER, ignore_attr = T)
  expect_equal(u_liberal, -penalty*details_liberal$FWER, ignore_attr = T)
  expect_true(u_conservative > 0)
  expect_true(u_liberal < 0)
})

# TODO: At the end, we should perhaps hardcode all baskwrap results --> so that
# the test is sensitive to changes in baskwrap code.
