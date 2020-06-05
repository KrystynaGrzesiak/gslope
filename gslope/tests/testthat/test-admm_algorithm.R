
test_that("admm returns symmetric matrix", {
  sample_cov = cov(scale(mtcars))
  p = nrow(sample_cov)
  lambda = rep(1, p * (p - 1)/2)
  admm_results = ADMM_algorithm(sample_cov, lambda, 1.1, 1e5, 1e-4)
  expect_true(isSymmetric.matrix(round(admm_results[[1]], 8)))
})
