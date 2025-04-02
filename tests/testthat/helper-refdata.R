# GENERAL PARAMETERS
design4 <- baskwrap::setup_fujikawa_x(k = 4, shape1 = 1, shape2 = 1,
                                     p0 = 0.1, backend = "exact")
logbase <- 3
detail_params_fuj <- list(n = 15,
                          weight_fun = baskexact::weights_fujikawa,
                          logbase = logbase,
                          verbose = FALSE)
epsilon <- 1.5
tau <- 0.25
lambda_conservative <- 0.99
lambda_liberal <- 0.95
x_fuj_vconservative <- list(lambda = 0.999, epsilon = 2, tau = 0.75)
x_fuj_conservative <- list(lambda = lambda_conservative, epsilon = epsilon, tau = tau)
x_fuj_liberal <- list(lambda = lambda_liberal, epsilon = epsilon, tau = tau)
p0 <- c(0.1, 0.1, 0.1, 0.1)
p1_low <- c(0.1, 0.1, 0.2, 0.21)
p1_high <- c(0.1, 0.1, 0.25, 0.3)
