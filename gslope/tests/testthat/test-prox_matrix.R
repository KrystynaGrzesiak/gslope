
test_that("output matrix is symmetric", {
  matrix_in = cov(mtcars)
  lambda = rep(1, ncol(matrix_in) * (ncol(matrix_in) - 1)/2)
  expect_true(isSymmetric.matrix(prox_matrix(matrix_in, lambda)))
})


test_that("input matrix must be square", {
  matrix_in = cov(mtcars)[,-1]
  lambda = rep(1, ncol(matrix_in) * (ncol(matrix_in) - 1)/2)
  expect_error(prox_matrix(matrix_in, lambda))
})


test_that("output matrix has the same dimension as input", {
  matrix_in = cov(mtcars)
  lambda = rep(1, ncol(matrix_in) * (ncol(matrix_in) - 1)/2)
  expect_equal(dim(matrix_in), dim(prox_matrix(matrix_in, lambda)))
})
