
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


test_that("gslope returns list of 8 elements", {
  gslope_result = gslope(scale(mtcars), scaled = TRUE)
  expect_length(gslope_result, 8)
})


