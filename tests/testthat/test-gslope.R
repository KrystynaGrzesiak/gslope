
test_that("prepared lambda has the proper length", {
  sample_cov = cov(scale(mtcars))
  p = nrow(sample_cov)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, n)

  expect_length(prepare_lambda(lambda, p*(p-1)/2), sum(lower.tri(sample_cov, FALSE)))
})


test_that("prepare_lambda warns about too short lambda", {
  sample_cov = cov(scale(mtcars))
  p = nrow(sample_cov)
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, n)[-1]

  expect_warning(prepare_lambda(lambda, p*(p-1)/2))
})


test_that("prepare_lambda warns about too long lambda", {
  sample_cov = cov(scale(mtcars))
  p = nrow(sample_cov) - 1
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, n)

  expect_warning(prepare_lambda(lambda, p*(p-1)/2))
})


test_that("prepared lambda is sorted in descending order", {
  sample_cov = cov(scale(mtcars))
  n = nrow(mtcars)
  lambda = create_lambda(sample_cov, n)
  p = nrow(sample_cov)
  expect_true(!is.unsorted(rev(prepare_lambda(lambda, p*(p-1)/2))))
})


test_that("gslope returns an instance of the class gslope", {
  gslope_result = gslope(scale(mtcars), scaled = TRUE)
  expect_true(class(gslope_result) == "gslope")
  expect_length(gslope_result, 8)
})

