test_that("extreme borrowing cutoff is plausible", {
  detail_params <- list(n = 12, logbase = exp(1))
  lambda <- 0.8
  tau <- 0.5
  p1 <- c(design4$p0, design4$p0, 0.5, 0.5)
  eps_ex <- epsilon_extreme(design4, tau = 0.5, detail_params)
  mat_eps_ex <- baskwrap::weights_fujikawa_x(design4,
                        n = detail_params$n,
                        logbase = detail_params$logbase,
                        epsilon = eps_ex,
                        tau = tau)
  mat_gt_eps_ex <- baskwrap::weights_fujikawa_x(design4,
                                             n = detail_params$n,
                                             logbase = detail_params$logbase,
                                             epsilon = eps_ex + 0.5,
                                             tau = tau)
  mat_lt_eps_ex <- baskwrap::weights_fujikawa_x(design4,
                                                n = detail_params$n,
                                                logbase = detail_params$logbase,
                                                epsilon = eps_ex - 0.5,
                                                tau = tau)
  expect_equal(mat_eps_ex, mat_gt_eps_ex)
  expect_equal(mat_eps_ex[7, 6], 0)
  expect_true(any(mat_eps_ex != mat_lt_eps_ex))
  expect_gte(mat_lt_eps_ex[7, 6], tau)
})
