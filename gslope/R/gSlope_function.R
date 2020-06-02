source("gSlope_S3.R")
source("prox_for_class.R")

gSLOPE = function(data,
                  lambda = NULL,
                  alpha = 0.05,
                  Y = NULL,
                  mu = 1.1,
                  epsilon = 1e-4,
                  max_iter = 1e5){

  class(data) = "gSlope"
  precision_matrix = gSlope.ADMM(data, lambda, mu, Y, max_iter, epsilon, alpha)

  list(precision_matrix = precision_matrix,
       covariance_matrix = solve(precision_matrix),
       lambda = gSlope.prepare_lambda(data, lambda, alpha))
}

data = rnorm(50)
data = matrix(data, 5, 10)

gSLOPE(data)["precision_matrix"]
