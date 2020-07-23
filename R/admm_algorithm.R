prox_matrix = function(matrix_in, lambda) {
  out = matrix_in
  precision_entries = matrix_in[lower.tri(matrix_in, FALSE)]
  calculated_entries = suppressWarnings(SLOPE:::sorted_l1_prox(as.matrix(abs(precision_entries)),
                                                               lambda))
  out[lower.tri(out, FALSE)] = calculated_entries
  out[upper.tri(out, FALSE)] = calculated_entries
  out
}


#' @title ADMM algorithm for graphical SLOPE
#' @description  Executes ADMM algorithm to find precision matrix.
#' @param sample_cov variance-covariance matrix.
#' @param lambda vector of SLOPE regularizers.
#' @param mu correction for lambda scaling.
#' @param max_iter maximum number of iterations of ADMM algorithm. Default 10 000.
#' @param epsilon a value determining stop condition.
#' @keywords admm
#' @return \code{ADMM_algorithm} returns a list with components:
#' \itemize{
#' \item \code{precision_matrix} precision matrix revealing graph structure for the data.
#' \item \code{iterations} number of iterations before stop condition or \code{max_iter}.
#' }
#' @details ADMM algorithm is used to solve convex optimization problems by dividing them into smaller, easier to solve problems.
#' @examples
#' sample_cov = cov(scale(mtcars))
#' lambda = gslope::create_lambda(sample_cov, nrow(mtcars))
#' precision_matrix = ADMM_algorithm(sample_cov, lambda)
#' @export
#'



ADMM_algorithm = function(sample_cov, lambda, mu = 1.1,
                          max_iter = 1e4, epsilon = 1e-4) {
  if(!(nrow(sample_cov) == ncol(sample_cov))) stop("Covariance matrix must be square.")
  Z = sample_cov * 0
  Y = Z
  X = diag(nrow(sample_cov))

  for(iter in 1:max_iter) {
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
    dual_residual = norm(mu * (Y - Y_old), type = "F")

    if(primal_residual < epsilon & dual_residual < epsilon)
      break
  }
  list(precision_matrix = X,
       iterations = iter)
}
