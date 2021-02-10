context("Sampling")

set.seed(1)
test_that("sampling system", {
  sample <- sample_chains(50, 0.5, 3)
  expect_equal(sample$mu1, c(0.7548244, 0.2616238, 0.2842341), tol = 1e-7)
  expect_equal(sample$pi, c(0.2016819, 0.6870228, 0.6620051), tol = 1e-7)
  expect_equal(sample$phi, c(0.8983897, 0.3841037, 0.4068302), tol = 1e-7)
  expect_equal(sample$alpha, c(0.2048945, 0.9931723, 0.4088374), tol = 1e-7)
})
