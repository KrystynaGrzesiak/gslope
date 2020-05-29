source("admm_algorithm.R")
source("prepare_lambda.R")
source("create_lambda.R")


gSlope_ADMM = function(data,
                       lambda = NULL,
                       mu = 1.1,
                       Y = NULL,
                       maxIter = 1e5,
                       epsilon = 1e-4,
                       alpha = 0.05){

  p = ncol(data)
  n = nrow(data)

  sampleCovariance = cov(data)
  
  if(is.null(lambda))
    lambda = create_lambda(sampleCovariance, p, n, alpha)
    
  lambda = prepare_lambda(lambda, p)

  Z = sampleCovariance * 0
  if(is.null(Y))
    Y = Z
  X = diag(nrow = p)

  precision_matrix = ADMM_algorithm(sampleCovariance, lambda,
                                    mu, Y, Z, maxIter, epsilon)

  list(precision_matrix = precision_matrix,
       covariance_matrix = solve(precision_matrix),
       lambda = lambda)
}
