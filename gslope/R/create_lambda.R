
lambdaBH <- function(p, n, alpha = 0.05, twoLargestProd = 1) {

  pBH = p * (p - 1) / 2
  k = 1:pBH
  fractionSeq = qt(1 - alpha* k / (2 * pBH), n - 2) /sqrt(n - 2 + qt(1 - alpha * k / (2 * pBH), n - 2) ^2)

  fractionSeq = c(rep(fractionSeq[1], p), rep(fractionSeq, each = 2))

  twoLargestProd * fractionSeq
}


create_lambda = function(sampleCovariance, n) {
  twoLargestProd = prod( -sort( -diag(sampleCovariance), partial = 2)[1:2])
  lambda_seq = lambdaBH(p, n, alpha = 0.05, twoLargestProd)
}
