context("Matrix definition")

test_that("matrix is defined correctly", {
  expect_equal(define_NGM(0.5, 0.8, 0.7, 0.5),
               matrix(c(0.5, 0.325, 0.26, 0.5, 0.325, 0.26, 0.0, 0.350, 0.28),  nrow = 3, ncol = 3, byrow=TRUE))
  expect_equal(define_NGM(0.6, 0.8, 0.7, 0.1),
               matrix(c(0.4, 0.372, 0.2976, 0.6, 0.558, 0.4464, 0.0, 0.070, 0.0560),  nrow = 3, ncol = 3, byrow=TRUE))
  expect_equal(define_NGM_four(0.6, 0.8, 0.7, 0.1),
               matrix(c(0.4, 0.372, 0.372, 0.2976, 0.6, 0.540, 0.540, 0.4320, 0.0, 0.018, 0.018, 0.0144, 0.0, 0.070, 0.070, 0.0560),  nrow = 4, ncol = 4, byrow=TRUE))
})

test_that("analytic solution is defined correctly", {
  expect_equal(prop_missing_cases(0.5, 0.8, 0.7, 0.5), 0.397543, tolerance=1e-7)
  expect_equal(prop_missing_cases(0.6, 0.8, 0.7, 0.1), 0.3828116, tolerance=1e-3)
})
