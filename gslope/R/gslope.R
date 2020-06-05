
source("R\\admm_algorithm.R")
source("R\\lambda.R")


gslope = function(data,
                  scaled = FALSE,
                  lambda = NULL,
                  mu = 1.1,
                  max_iter = 1e5,
                  epsilon = 1e-4,
                  alpha = 0.05) {

  p = ncol(data)
  n = nrow(data)
  low_tri_size = p * (p - 1) / 2

  if(!scaled)
    data = scale(data)

  sample_cov = cov(data)

  if(is.null(lambda))
    lambda = create_lambda(sample_cov, p, n, alpha)

  lambda = prepare_lambda(lambda, p)

  ADMM_results = ADMM_algorithm(sample_cov, lambda,
                                    mu, max_iter, epsilon)

  precision_matrix = ADMM_results[[1]]
  precision_matrix[abs(precision_matrix) < epsilon] = 0 # rounding to 0 before scaling?

  scaled_precision_matrix = cov2cor(precision_matrix)

  list(precision_matrix = precision_matrix,
       covariance_matrix = solve(precision_matrix),
       scaled_precision_matrix = scaled_precision_matrix,
       lambda = lambda,
       iterations = ADMM_results[[2]])
}
