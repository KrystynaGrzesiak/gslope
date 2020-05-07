
prepare_lambda = function(lambda, p, penalizeDiagonal, entriesNumber){

lambda <- sort(lambda, decreasing = T)

if(length(lambda) == p^2) {
  lambda_part_seq = lambda[seq(p+1, length(lambda), by = 2)]
  if(penalizeDiagonal)
    lambda = c(lambda[1:p], lambda_part_seq) else
      lambda = lambda_part_seq
}
  
else if(length(lambda) < entriesNumber)
  lambda <- c(lambda, rep(0, entriesNumber - length(lambda)))
  
else if(length(lambda) > entriesNumber)
  lambda <- lambda[1:entriesNumber]
}
