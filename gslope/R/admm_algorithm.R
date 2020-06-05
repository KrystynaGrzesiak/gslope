
source("R\\prox_matrix.R")


ADMM_algorithm = function(sample_cov,
                          lambda,
                          mu,
                          max_iter,
                          epsilon) {

  Z = sample_cov * 0
  Y = Z
  X = diag(nrow(sample_cov))

  for(n in 1:max_iter) {
    C_tilde = Y - Z - sample_cov / mu
    C_eigen = eigen(C_tilde, symmetric = TRUE)
    C_eigen_val = C_eigen$val
    C_eigen_vec = C_eigen$vec
    F_mu = 1 / 2 * diag(C_eigen_val + sqrt(C_eigen_val * C_eigen_val + 4 / mu))

    X = C_eigen_vec %*% F_mu %*% t(C_eigen_vec)

    Y_old = Y
    Y = prox_matrix(X + Z, lambda / mu)

    Z = Z + mu * (X - Y)

    primal_residual = norm(X - Y, type = "F")
    dual_residual   = norm(mu * (Y - Y_old), type = "F")

    if(primal_residual < epsilon & dual_residual < epsilon)
      break
  }

  list(X, n)
}
