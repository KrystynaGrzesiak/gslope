source("admm_algorithm.R")
source("prepare_lambda.R")
source("create_lambda.R")

gSlope_ADMM <- function(data, 
                       lambda = NULL, 
                       penalizeDiagonal = FALSE,
                       mu = 1.1, 
                       Y = NULL,
                       maxIter = 1e5, 
                       epsilon = 1e-4) {

  p <- ncol(data)
  n <- nrow(data)
  
  sampleCovariance = cov(data)
  
  entriesNumber <- sum(1:(p-!penalizeDiagonal))
  
  lambda = create_lambda(sampleCovariance, n)
  
  lambda = prepare_lambda(lambda, p, penalizeDiagonal, entriesNumber)
  
  Z <- sampleCovariance*0
  if(is.null(Y))
    Y <- Z 
  X <- diag(nrow = p)
  
  precision_matrix = ADMM_algorithm(sampleCovariance, lambda, penalizeDiagonal, mu, Y, Z, maxIter, epsilon)
  
  return(list(precision_matrix = precision_matrix,
              covariance_matrix = solve(precision_matrix),
              lambda = lambda))
}
