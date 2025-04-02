# This is code to generate reference data sets

# SETUP
# Load all functions, e.g. using devtools::load_all()

# Generate results for comparison
details_vconservative <- baskwrap::get_details(design4,
                                               n = detail_params_fuj$n,
                                               p1 = p1_high,
                                               lambda = x_fuj_vconservative$lambda,
                                               epsilon = x_fuj_vconservative$epsilon,
                                               tau = x_fuj_vconservative$tau,
                                               logbase = logbase)
saveRDS(details_vconservative, test_path(path_refdata_rel,
                                         "details_vconservative.RDS"))
details_vconservative_null <- baskwrap::get_details(design4,
                                                    n = detail_params_fuj$n,
                                                    p1 = p0,
                                                    lambda = x_fuj_vconservative$lambda,
                                                    epsilon = x_fuj_vconservative$epsilon,
                                                    tau = x_fuj_vconservative$tau,
                                                    logbase = logbase,
                                                    verbose = F)
saveRDS(details_vconservative_null, test_path(path_refdata_rel,
                                         "details_vconservative_null.RDS"))
details_conservative <- baskwrap::get_details(design4,
                                              n = detail_params_fuj$n,
                                              p1 = p1_high,
                                              lambda = lambda_conservative,
                                              epsilon = epsilon,
                                              tau = tau,
                                              logbase = logbase)
saveRDS(details_conservative, test_path(path_refdata_rel,
                    "details_conservative.RDS"))
details_conservative_lowpower <- baskwrap::get_details(design4,
                                                       n = detail_params_fuj$n,
                                                       p1 = p1_low,
                                                       lambda = lambda_conservative,
                                                       epsilon = epsilon,
                                                       tau = tau,
                                                       logbase = logbase)
saveRDS(details_conservative_lowpower, test_path(path_refdata_rel,
                    "details_conservative_lowpower.RDS"))
details_liberal_alt <- baskwrap::get_details(design4,
                                             n = detail_params_fuj$n,
                                             p1 = p1_high,
                                             lambda = lambda_liberal,
                                             epsilon = epsilon,
                                             tau = tau,
                                             logbase = logbase)
saveRDS(details_liberal_alt, test_path(path_refdata_rel,
                    "details_liberal_alt.RDS"))
details_liberal_null <- baskwrap::get_details(design4,
                                              n = detail_params_fuj$n,
                                              p1 = p0,
                                              lambda = lambda_liberal,
                                              epsilon = epsilon,
                                              tau = tau,
                                              logbase = logbase,
                                              verbose = F)
saveRDS(details_liberal_null, test_path(path_refdata_rel,
                    "details_liberal_null.RDS"))
