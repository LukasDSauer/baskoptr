test_that("get_scenarios returns desired scenario", {
  scenarios <- get_scenarios(k = 4, p0 = 0.3, p1 = 0.5, by = 2)
  expect_equal(scenarios$k, 4)
  expect_equal(scenarios$p0, 0.3)
  expect_equal(scenarios$p1s,  rbind(c(0.3,  0.3,  0.3,  0.3),
                                     c(0.3,  0.3,  0.5,  0.5),
                                     c(0.5,  0.5,  0.5,  0.5)))
})
