
test_that("lambda series is sorted", {
  sample_cov = cov(scale(mtcars))
  p = ncol(mtcars)
  n = nrow(mtcars)
  expect_true(!is.unsorted(rev(create_lambda(sample_cov, p*(p-1)/2, n))))
})


test_that("prepared lambda has the proper length", {
  sample_cov = cov(scale(mtcars))
  p = ncol(mtcars)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, p*(p-1)/2, n)

  expect_equal(length(prepare_lambda(lambda, p*(p-1)/2)), sum(lower.tri(sample_cov, FALSE)))
})


test_that("prepared lambda has the proper length", {
  sample_cov = cov(scale(mtcars))
  p = ncol(mtcars)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, p*(p-1)/2, n)[-1]

  expect_message(prepare_lambda(lambda, p*(p-1)/2))
})
