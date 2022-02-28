context("Solution")

test_that("difference is correct", {
  expect_equal(diff_funct_three_state(0.3, 0.7, 0.47, 0.59, 1.5), 0.3706342)
  expect_equal(diff_funct_three_state(pi = 0.3, phi = 0.7, alpha = 0.47, r1=0.59, r2=1.5), 0.3706342)
  expect_equal(diff_funct_three_state(0.5, 0.2, 0.5, 0.7, 2), 0.7120469)
})

test_that("compute correct mu and alpha", {
  expect_equal(compute_alpha_mu1(0.3, 0.7, 0.59, 1.5), NA)
  sol1 <- compute_alpha_mu1(0.3, 0.7, 0.59, 3)
  expect_equal(sol1$alpha, 0.4772321)
  expect_equal(sol1$mu1, 0.6460157)
})
