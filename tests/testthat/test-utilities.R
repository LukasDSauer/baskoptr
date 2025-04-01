details_vconservative <- baskwrap::get_details(design4,
                                              n = detail_params_fuj$n,
                                              p1 = p1_high,
                                              lambda = x_fuj_vconservative$lambda,
                                              epsilon = x_fuj_vconservative$epsilon,
                                              tau = x_fuj_vconservative$tau,
                                              logbase = logbase)
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
details_liberal_alt <- baskwrap::get_details(design4,
                                         n = detail_params_fuj$n,
                                         p1 = p1_high,
                                         lambda = lambda_liberal,
                                         epsilon = epsilon,
                                         tau = tau,
                                         logbase = logbase)
details_liberal_null <- baskwrap::get_details(design4,
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
  set.seed(123)
  penalty <- 3
  thresh <- 0.1
  u_conservative <- u_ewp(design = design4,
              x = x_fuj_conservative,
              detail_params = detail_params_fuj,
              p1 = p1_high,
              threshold = thresh,
              penalty = penalty,
              report_details = TRUE)
  u_conservative_sim <- u_ewp(design = baskwrap::set_backend(design4, "sim"),
                              x = x_fuj_conservative,
                              detail_params = c(detail_params_fuj,
                                                list(iter = 10000)),
                              p1 = p1_high,
                              threshold = thresh,
                              penalty = penalty,
                              report_details = TRUE)
  u_liberal <- u_ewp(design = design4,
                          x = x_fuj_liberal,
                          detail_params = detail_params_fuj,
                          p1 = p1_high,
                          threshold = thresh,
                          penalty = penalty,
                          report_details = TRUE)
  u_liberal_sim <- u_ewp(design = baskwrap::set_backend(design4, "sim"),
                     x = x_fuj_liberal,
                     detail_params = detail_params_fuj,
                     p1 = p1_high,
                     threshold = thresh,
                     penalty = penalty,
                     report_details = TRUE)
  expect_equal(u_conservative, details_conservative$EWP, ignore_attr = T)
  expect_equal(u_conservative_sim, details_conservative$EWP, ignore_attr = T,
               tolerance = 0.05)
  expect_equal(u_liberal, -penalty*details_liberal_null$FWER, ignore_attr = T)
  expect_equal(u_liberal_sim, -penalty*details_liberal_null$FWER,
               ignore_attr = TRUE, tolerance = 0.05)
  expect_true(u_conservative > 0)
  expect_true(u_liberal < 0)
})

test_that("u_ecd() delivers the expected results", {
  set.seed(456)
  penalty <- 2
  thresh <- 0.1
  u_conservative <- u_ecd(design = design4,
                          x = x_fuj_conservative,
                          detail_params = detail_params_fuj,
                          p1 = p1_low,
                          threshold = thresh,
                          penalty = penalty,
                          report_details = TRUE)
  u_conservative_sim <- u_ecd(design = baskwrap::set_backend(design4, "sim"),
                          x = x_fuj_conservative,
                          detail_params = detail_params_fuj,
                          p1 = p1_low,
                          threshold = thresh,
                          penalty = penalty,
                          report_details = TRUE)
  u_liberal <- u_ecd(design = design4,
                     x = x_fuj_liberal,
                     detail_params = detail_params_fuj,
                     p1 = p1_high,
                     threshold = thresh,
                     penalty = penalty,
                     report_details = TRUE)
  u_liberal_sim <- u_ecd(design = baskwrap::set_backend(design4, "sim"),
                     x = x_fuj_liberal,
                     detail_params = detail_params_fuj,
                     p1 = p1_high,
                     threshold = thresh,
                     penalty = penalty,
                     report_details = TRUE)
  expect_equal(u_conservative, details_conservative_lowpower$ECD,
               ignore_attr = T)
  expect_equal(u_conservative_sim, details_conservative_lowpower$ECD,
               ignore_attr = T, tolerance = 0.001)
  expect_equal(u_liberal, -penalty*details_liberal_null$FWER, ignore_attr = T)
  expect_equal(u_liberal_sim, -penalty*details_liberal_null$FWER,
               ignore_attr = T, tolerance = 0.05)
  expect_true(u_conservative > 0)
  expect_true(u_liberal < 0)
})
# Calculating utility values for u_2ewp - We need these later
set.seed(789)
penalty1_2ewp <- 1
penalty2_2ewp <- 2
threshold_2ewp <- 0.2
u_vconservative_2ewp <- u_2ewp(design = design4,
                               x = x_fuj_vconservative,
                               detail_params = detail_params_fuj,
                               p1 = p1_high,
                               threshold = threshold_2ewp,
                               penalty1 = penalty1_2ewp,
                               penalty2 = penalty2_2ewp,
                               report_details = TRUE)
u_vconservative_2ewp_null <- u_2ewp(design = design4,
                                     x = x_fuj_vconservative,
                                     detail_params = detail_params_fuj,
                                     p1 = p0,
                                     threshold = threshold_2ewp,
                                     penalty1 = penalty1_2ewp,
                                     penalty2 = penalty2_2ewp,
                                     report_details = TRUE)
u_liberal_2ewp <- u_2ewp(design = design4,
                         x = x_fuj_liberal,
                         detail_params = detail_params_fuj,
                         p1 = p1_high,
                         threshold = threshold_2ewp,
                         penalty1 = penalty1_2ewp,
                         penalty2 = penalty2_2ewp,
                         report_details = TRUE)
u_vconservative_2ewp_sim <- u_2ewp(design = baskwrap::set_backend(design4,
                                                                  "sim"),
                                   x = x_fuj_vconservative,
                                   detail_params = detail_params_fuj,
                                   p1 = p1_high,
                                   threshold = threshold_2ewp,
                                   penalty1 = penalty1_2ewp,
                                   penalty2 = penalty2_2ewp,
                                   report_details = TRUE)
u_vconservative_2ewp_null_sim <- u_2ewp(design = baskwrap::set_backend(design4,
                                                                       "sim"),
                                        x = x_fuj_vconservative,
                                        detail_params = detail_params_fuj,
                                        p1 = p0,
                                        threshold = threshold_2ewp,
                                        penalty1 = penalty1_2ewp,
                                        penalty2 = penalty2_2ewp,
                                        report_details = TRUE)
u_liberal_2ewp_sim <- u_2ewp(design = baskwrap::set_backend(design4, "sim"),
                             x = x_fuj_liberal,
                             detail_params = detail_params_fuj,
                             p1 = p1_high,
                             threshold = threshold_2ewp,
                             penalty1 = penalty1_2ewp,
                             penalty2 = penalty2_2ewp,
                             report_details = TRUE)
test_that("u_2ewp() delivers the expected results", {
  # Reference values
  u_ref_vconservative_2ewp <-
    details_vconservative$EWP - penalty1_2ewp*details_vconservative$FWER
  u_ref_vconservative_2ewp_null <-
    -penalty1_2ewp*attr(u_vconservative_2ewp_null, "details")$p1$FWER
  u_ref_liberal_2ewp <-
    details_liberal_alt$EWP -
    penalty1_2ewp*details_liberal_alt$FWER -
    penalty2_2ewp*(details_liberal_alt$FWER - threshold_2ewp)
  # Comparison
  expect_equal(u_vconservative_2ewp,
               u_ref_vconservative_2ewp,
               ignore_attr = T)
  expect_equal(u_vconservative_2ewp_null,
               u_ref_vconservative_2ewp_null,
               ignore_attr = T)
  expect_equal(u_liberal_2ewp, u_ref_liberal_2ewp,
               ignore_attr = T)
  expect_equal(u_vconservative_2ewp_sim,
               u_ref_vconservative_2ewp,
               ignore_attr = T,
               tolerance = 0.05)
  expect_equal(u_vconservative_2ewp_null_sim,
               u_ref_vconservative_2ewp_null,
               ignore_attr = T,
               tolerance = 0.01)
  expect_equal(u_liberal_2ewp_sim,
               u_ref_liberal_2ewp,
               ignore_attr = T,
               tolerance = 0.09)
  expect_true(u_vconservative_2ewp > 0)
  expect_true(u_liberal_2ewp < 0)
})

test_that("u_2pow() delivers the expected results", {
  set.seed(1011)
  penalty1 <- 1
  penalty2 <- 3
  threshold <- 0.1
  # References
  u_ref_vconservative <- details_vconservative$Rejection_Probabilities[3] +
    details_vconservative$Rejection_Probabilities[4] +
    - penalty1*(details_vconservative$Rejection_Probabilities[1] +
                  details_vconservative$Rejection_Probabilities[2])
  u_ref_liberal <- details_liberal_alt$Rejection_Probabilities[3] +
    details_liberal_alt$Rejection_Probabilities[4] -
    penalty1*(details_liberal_alt$Rejection_Probabilities[1] +
                details_liberal_alt$Rejection_Probabilities[2]) -
    penalty2*(
      details_liberal_alt$Rejection_Probabilities[1] +
        details_liberal_alt$Rejection_Probabilities[2] -
        2*threshold)
  # Function results
  u_vconservative <- u_2pow(design = design4,
                            x = x_fuj_vconservative,
                            detail_params = detail_params_fuj,
                            p1 = p1_high,
                            threshold = threshold,
                            penalty1 = penalty1,
                            penalty2 = penalty2,
                            report_details = TRUE)
  u_liberal <- u_2pow(design = design4,
                      x = x_fuj_liberal,
                      detail_params = detail_params_fuj,
                      p1 = p1_high,
                      threshold = threshold,
                      penalty1 = penalty1,
                      penalty2 = penalty2,
                      report_details = TRUE)
  u_vconservative_sim <- u_2pow(design = baskwrap::set_backend(design4,
                                                               "sim"),
                            x = x_fuj_vconservative,
                            detail_params = detail_params_fuj,
                            p1 = p1_high,
                            threshold = threshold,
                            penalty1 = penalty1,
                            penalty2 = penalty2,
                            report_details = TRUE)
  u_liberal_sim <- u_2pow(design = baskwrap::set_backend(design4,
                                                         "sim"),
                      x = x_fuj_liberal,
                      detail_params = detail_params_fuj,
                      p1 = p1_high,
                      threshold = threshold,
                      penalty1 = penalty1,
                      penalty2 = penalty2,
                      report_details = TRUE)
  # Comparisons
  expect_equal(u_vconservative, u_ref_vconservative, ignore_attr = T)
  expect_equal(u_liberal, u_ref_liberal, ignore_attr = T)
  expect_equal(u_vconservative_sim, u_ref_vconservative, ignore_attr = T,
               tolerance = 0.05)
  expect_equal(u_liberal_sim, u_ref_liberal, ignore_attr = T,
               tolerance = 0.05)
  expect_true(u_vconservative > 0)
  expect_true(u_liberal < 0)
})

test_that("u_avg() delivers the expected results", {
  utility_params_2ewp <- list(penalty1 = penalty1_2ewp,
                              penalty2 = penalty2_2ewp,
                              threshold = threshold_2ewp)
  u_ref <- (u_vconservative_2ewp_null + u_vconservative_2ewp)/2
  u_val <- u_avg(design = design4,
                x = x_fuj_vconservative,
                detail_params = detail_params_fuj,
                utility = u_2ewp,
                utility_params = utility_params_2ewp,
                p1s = rbind(p1_high, p0))
  u_val_sim <- u_avg(design = baskwrap::set_backend(design4, "sim"),
                 x = x_fuj_vconservative,
                 detail_params = detail_params_fuj,
                 utility = u_2ewp,
                 utility_params = utility_params_2ewp,
                 p1s = rbind(p1_high, p0))
  expect_equal(u_val, u_ref, ignore_attr = T)
  expect_equal(u_val_sim, u_ref, ignore_attr = T, tolerance = 0.05)
})

test_that("u_bnd() delivers the expected results", {
  utility_params_2ewp <- list(p1 = p1_high,
                              penalty1 = penalty1_2ewp,
                              penalty2 = penalty2_2ewp,
                              threshold = threshold_2ewp)
  u_val_inbnd <- u_bnd(design = design4,
                 x = x_fuj_vconservative,
                 detail_params = detail_params_fuj,
                 utility = u_2ewp,
                 utility_params = utility_params_2ewp,
                 lower = list(lambda = 0.5, epsilon = 0.5, tau = 0.0001),
                 upper = list(lambda = 1, epsilon = 5, tau = 0.9999))
  u_val_outofbnd <- u_bnd(design = design4,
                       x = x_fuj_vconservative,
                       detail_params = detail_params_fuj,
                       utility = u_2ewp,
                       utility_params = utility_params_2ewp,
                       lower = list(lambda = 0.5, epsilon = 0.5, tau = 0.0001),
                       upper = list(lambda = 0.95, epsilon = 5, tau = 0.9999))
  expect_equal(u_val_inbnd,
               u_vconservative_2ewp,
               ignore_attr = T)
  expect_equal(u_val_outofbnd,
               NA_real_,
               ignore_attr = T)
})

# TODO: At the end, we should perhaps hardcode all baskwrap results --> so that
# the test is sensitive to changes in baskwrap code.
# Easiest way: just save the objects generated by baskwrap.
