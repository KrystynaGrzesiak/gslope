#' @title Penalty parameters for graphical SLOPE
#' @description  Computes penalty parameters \eqn{\lambda} for graphical SLOPE.
#' @param sample_cov a sample variance-covariance matrix.
#' @param n number of observations.
#' @param alpha significance level.
#' @keywords lambda, graphical slope, penalty
#' @return \code{create_lambda()} returns a vector of the length \eqn{p * (p-1)/2}, which is equal
#' to number of elements in lower (upper) triangle of a variance-covariance matrix excluding diagonal.
#' @details The Lambda series are computed based on Benjamini-Hochberg's correction.
#' @examples
#' sample_cov = cov(scale(mtcars))
#' create_lambda(sample_cov, nrow(mtcars))
#' @export


create_lambda = function(sample_cov, n, alpha = 0.05) {
  p = ncol(sample_cov)
  low_tri_size = p * (p - 1) / 2
  k = 1:low_tri_size

  two_largest_prod = prod( -sort( -diag(sample_cov), partial = 2)[1:2])
  fraction_seq = qt(1 - alpha * k / (2 * low_tri_size), n - 2) /
    sqrt(n - 2 + qt(1 - alpha * k / (2 * low_tri_size), n - 2) ^2)

  two_largest_prod * fraction_seq
}
