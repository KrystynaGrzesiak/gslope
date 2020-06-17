
test_that("created lambda series is sorted in descending order", {
  sample_cov = cov(scale(mtcars))
  n = nrow(mtcars)
  expect_true(!is.unsorted(rev(create_lambda(sample_cov, n))))
})


test_that("created lambda has the proper length", {
  sample_cov = cov(scale(mtcars))
  n = nrow(mtcars)
  expect_length(create_lambda(sample_cov, n), ncol(sample_cov) * (ncol(sample_cov)-1)/2)
})
