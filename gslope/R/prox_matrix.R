require(SLOPE)

OWL1fastprox <- function(entries, 
                         lambdaSeries)
{
  return(prox_sorted_L1(entries, lambdaSeries, method = c("c")))
}


matrixOWL1prox <- function(matrix, 
                           lambdaSeries, #must be sorted!
                           penalizeDiagonal = FALSE)
{
  out <- matrix
  
  precisionEntries <- matrix[lower.tri(matrix, penalizeDiagonal)]
  
  calculatedEntries <- OWL1fastprox(abs(precisionEntries), lambdaSeries)
  
  out[lower.tri(out, penalizeDiagonal)] <- calculatedEntries
  out <- t(out)
  out[lower.tri(out, penalizeDiagonal)] <- calculatedEntries
  
  return(out)
}