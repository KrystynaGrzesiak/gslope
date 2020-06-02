require(SLOPE)


gSlope.get_p = function(data)
  ncol(data)


gSlope.get_n = function(data)
  nrow(data)


gSlope.get_sample_covariance = function(data)
  cov(data)


gSlope.get_two_largest_prod = function(data)
  prod( -sort( -diag(gSlope.get_sample_covariance(data)), partial = 2)[1:2])


gSlope.low_tri_size = function(data){
  p = gSlope.get_p(data)
  p * (p - 1) / 2
}


gSlope.lambda_BH = function(data, alpha){

  p = gSlope.get_p(data)
  n = gSlope.get_n(data)

  pBH = p * (p - 1) / 2
  k = 1:pBH

  fractionSeq = qt(1 - alpha * k / (2 * pBH), n - 2) /
    sqrt(n - 2 + qt(1 - alpha * k / (2 * pBH), n - 2) ^2)

  gSlope.get_two_largest_prod(data) * fractionSeq
}


gSlope.prepare_lambda = function(data, lambda, alpha){

  if(is.null(lambda)){
    lambda = gSlope.lambda_BH(data, alpha)
    lambda = sort(lambda, decreasing = T)
  } else{
      lambda = sort(lambda, decreasing = T)
      low_tri_size = gSlope.low_tri_size(data)

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
    }
  lambda
}


gSlope.ADMM = function(data, lambda, mu, Y, max_iter, epsilon, alpha){

  sample_cov = gSlope.get_sample_covariance(data)
  lambda = gSlope.prepare_lambda(data, lambda, alpha)

  Z = sample_cov * 0
  if(is.null(Y))
    Y = Z
  X = diag(gSlope.get_p(data))

  for(n in 1:max_iter) {
    C_tilde = Y - Z - sample_cov / mu
    C_eigen = eigen(C_tilde, symmetric = TRUE)
    C_eigen_val = C_eigen$val
    C_eigen_vec = C_eigen$vec
    F_mu = 1 / 2 * diag(C_eigen_val + sqrt(C_eigen_val * C_eigen_val + 4 / mu))
    X = C_eigen_vec %*% F_mu %*% t(C_eigen_vec)

    Y_old = Y
    Y = prox_matrix(X + Z, lambda/mu)

    Z = Z + mu * (X - Y)

    primalResidual = norm(X - Y, type = "F")
    dualResidual   = norm(mu * (Y - Y_old), type = "F")

    if(primalResidual < epsilon & dualResidual < epsilon)
      break
  }

  X[abs(X) < epsilon] = 0
  X
}

