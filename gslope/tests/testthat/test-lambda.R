
test_that("lambda series is sorted", {
  sample_cov = cov(mtcars)
  p = ncol(mtcars)
  n = nrow(mtcars)
  expect_true(!is.unsorted(rev(create_lambda(sample_cov, p, n))))
})


test_that("prepared lambda has the proper length", {
  sample_cov = cov(mtcars)
  p = ncol(mtcars)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, p, n)

  expect_equal(length(prepare_lambda(lambda, p)), sum(lower.tri(sample_cov)))
})


test_that("prepared lambda has the proper length", {
  sample_cov = cov(mtcars)
  p = ncol(mtcars)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, p, n)[-1]

  expect_message(prepare_lambda(lambda, p))
})
