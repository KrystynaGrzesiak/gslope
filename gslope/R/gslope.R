
source("R\\admm_algorithm.R")
source("R\\lambda.R")


gslope = function(data,
                  lambda = NULL,
                  mu = 1.1,
                  Y = NULL,
                  max_iter = 1e5,
                  epsilon = 1e-4,
                  alpha = 0.05) {

  p = ncol(data)
  n = nrow(data)

  sample_cov = cov(data)

  if(is.null(lambda))
    lambda = create_lambda(sample_cov, p, n, alpha)

  lambda = prepare_lambda(lambda, p)

  Z = sample_cov * 0
  if(is.null(Y))
    Y = Z
  X = diag(nrow = p)

  ADMM_results = ADMM_algorithm(sample_cov, lambda,
                                    mu, Y, Z, max_iter, epsilon)

  precision_matrix = ADMM_results[[1]]
  scaled_precision_matrix = cov2cor(precision_matrix) # is it correct?

  list(precision_matrix = precision_matrix,
       covariance_matrix = solve(precision_matrix),
       scaled_precision_matrix = scaled_precision_matrix,
       lambda = lambda,
       iterations = ADMM_results[[2]])
}
