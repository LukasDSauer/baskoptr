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
