
require(SLOPE)


OWL1fastprox = function(entries,
                         lambdaSeries)
{
  prox_sorted_L1(entries, lambdaSeries, method = c("c"))
}


matrixOWL1prox = function(matrix,
                           lambdaSeries)
{
  out = matrix

  precisionEntries = matrix[lower.tri(matrix, FALSE)]

  calculatedEntries = OWL1fastprox(abs(precisionEntries), lambdaSeries)

  out[lower.tri(out, FALSE)] = calculatedEntries
  out = t(out)
  out[lower.tri(out, FALSE)] = calculatedEntries

  out
}
