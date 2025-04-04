
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baskoptr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/LukasDSauer/baskoptr/graph/badge.svg)](https://app.codecov.io/gh/LukasDSauer/baskoptr)
[![R-CMD-check](https://github.com/LukasDSauer/baskoptr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LukasDSauer/baskoptr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of **baskoptr** is to supply a unified framework for optimizing
basket trial designs. To this end, the package supplies several utility
functions and also a function for executing optimization algorithms on
basket trial designs.

## Installation

You can install the development version of baskoptr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("LukasDSauer/baskoptr")
```

## Example

In the following example, we optimize Fujikawa et al.’s basket trial
design with respect to the experiment-wise power utility function using
the simulated annealing algorithm.

``` r
library(baskoptr)
# Optimizing a three-basket trial design using Fujikawa's beta-binomial
# sharing approach
design <- baskwrap::setup_fujikawa_x(k = 3, shape1 = 1, shape2 = 1,
                                     p0 = 0.2, backend = "exact")
detail_params <- list(p1 = c(0.5, 0.2, 0.2),
                      n = 20,
                      weight_fun = baskexact::weights_fujikawa,
                      logbase = exp(1),
                      verbose = FALSE)
utility_params <- list(penalty = 1, thresh = 0.1)
opt_design_gen(design = design,
               utility = u_ewp,
               algorithm = optimizr::simann,
               detail_params = detail_params,
               utility_params = utility_params,
               algorithm_params = list(par = c(lambda = 0.99,
                                               epsilon = 2,
                                               tau = 0.5),
                                       lower = c(lambda = 0.001,
                                                 epsilon = 1,
                                                 tau = 0.001),
                                       upper = c(lambda = 0.999,
                                                 epsilon = 10,
                                                 tau = 0.999),
                                       control = list(maxit = 10,
                                                      temp = 10,
                                                      fnscale = -1,
                                                      REPORT = -1)))
#> $par
#>  lambda epsilon     tau 
#>    0.99    2.00    0.50 
#> 
#> $value
#> [1] 0.8036443
```
