
lambdaBH <- function(p, n, alpha = 0.05, twoLargestProd = 1) {

  pBH = p * (p - 1) / 2
  k = 1:pBH
  fractionSeq = qt(1 - alpha * k / (2 * pBH), n - 2) /sqrt(n - 2 + qt(1 - alpha * k / (2 * pBH), n - 2) ^2)

  twoLargestProd * fractionSeq
}


create_lambda = function(sampleCovariance, p, n, alpha = 0.05) {
  twoLargestProd = prod( -sort( -diag(sampleCovariance), partial = 2)[1:2])
  lambdaBH(p, n, alpha, twoLargestProd)
}


prepare_lambda = function(lambda, p) {
  
  entriesNumber = p * (p - 1) / 2 
  
  lambda = sort(lambda, decreasing = T)
  
  if(length(lambda) < entriesNumber){
    message("The lambda length is less than ncol(data) * (ncol(data)-1)/2. 
        Zeros will be added.")
    lambda = c(lambda, rep(0, entriesNumber - length(lambda)))
  }
  
  else if(length(lambda) > entriesNumber) {
    message("The lambda length is greater than ncol(data) * (ncol(data)-1)/2. 
        Lambda will be cut to the proper length.")
    lambda = lambda[1:entriesNumber]
  }
  
  lambda
}
