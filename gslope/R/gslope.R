

source("R\\admm_algorithm.R")
source("R\\create_lambda.R")


prepare_lambda = function(lambda, low_tri_size) {
  lambda = sort(lambda, decreasing = T)
  if(length(lambda) < low_tri_size) {
    warning("The length of lambda is less than ncol(data) * (ncol(data)-1)/2.
        Zeros will be added.", immediate. = FALSE)
    lambda = c(lambda, rep(0, low_tri_size - length(lambda)))
  }
  else if(length(lambda) > low_tri_size) {
    warning("The length of lambda is greater than ncol(data) * (ncol(data)-1)/2.
        Lambda will be cut to the proper length.", immediate. = FALSE)
    lambda = lambda[1:low_tri_size]
  }
  lambda
}


gslope = function(data, lambda = gslope::create_lambda(sample_cov, nrow(data), alpha),
                  sample_cov = cov(data), scaled = FALSE, mu = 1.1,
                  max_iter = 1e5, epsilon = 1e-4, alpha = 0.05) {
  #prepare parameters:
  p = ncol(data)
  lambda = prepare_lambda(lambda, p)
  if(!scaled) {data = scale(data)}

  ADMM_results = ADMM_algorithm(sample_cov, lambda, mu, max_iter, epsilon)

  precision_matrix = ADMM_results[[1]]
  precision_matrix[abs(precision_matrix) < epsilon] = 0 # rounding to 0 before scaling?
  scaled_precision_matrix = cov2cor(precision_matrix)

  list(precision_matrix = precision_matrix,
       covariance_matrix = solve(precision_matrix),
       scaled_precision_matrix = scaled_precision_matrix,
       lambda = lambda,
       iterations = ADMM_results[[2]])
}
