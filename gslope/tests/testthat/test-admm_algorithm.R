
test_that("prox_matrix returns symmetric matrix", {
  matrix_in = cov(scale(mtcars))
  lambda = rep(1, ncol(matrix_in) * (ncol(matrix_in) - 1)/2)
  expect_true(isSymmetric.matrix(prox_matrix(matrix_in, lambda)))
})


test_that("prox_matrix output has the same dimension as input", {
  matrix_in = cov(scale(mtcars))
  lambda = rep(1, ncol(matrix_in) * (ncol(matrix_in) - 1)/2)
  expect_equal(dim(matrix_in), dim(prox_matrix(matrix_in, lambda)))
})


test_that("admm returns symmetric matrix", {
  sample_cov = cov(scale(mtcars))
  p = nrow(sample_cov)
  lambda = rep(1, p * (p - 1)/2)
  admm_results = ADMM_algorithm(sample_cov, lambda, 1.1, 1e5, 1e-4)
  expect_true(isSymmetric.matrix(round(admm_results[[1]], 8)))
})
