
source("R\\admm_algorithm.R")
source("R\\lambda.R")


gslope = function(data, lambda = create_lambda(cov(data), ncol(data),
                                               nrow(data), alpha), scaled = FALSE
                  mu = 1.1, max_iter = 1e5, epsilon = 1e-4, alpha = 0.05) {
  #prepare parameters:
  p = ncol(data)
  sample_cov = cov(data)
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




