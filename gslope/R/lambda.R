
create_lambda = function(sample_cov,
                         p,
                         n,
                         alpha = 0.05) {

  low_tri_size = p * (p - 1) / 2
  k = 1:low_tri_size

  two_largest_prod = prod( -sort( -diag(sample_cov), partial = 2)[1:2])
  fraction_seq = qt(1 - alpha * k / (2 * low_tri_size), n - 2) /
    sqrt(n - 2 + qt(1 - alpha * k / (2 * low_tri_size), n - 2) ^2)

  two_largest_prod * fraction_seq
}


prepare_lambda = function(lambda, p) {

  low_tri_size = p * (p - 1) / 2

  lambda = sort(lambda, decreasing = T)

  if(length(lambda) < low_tri_size){
    message("The lambda length is less than ncol(data) * (ncol(data)-1)/2.
        Zeros will be added.")
    lambda = c(lambda, rep(0, low_tri_size - length(lambda)))
  }

  else if(length(lambda) > low_tri_size) {
    message("The lambda length is greater than ncol(data) * (ncol(data)-1)/2.
        Lambda will be cut to the proper length.")
    lambda = lambda[1:low_tri_size]
  }

  lambda
}
