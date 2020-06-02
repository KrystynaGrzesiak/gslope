
prox_matrix = function(matrix, lambda){

  out = matrix
  precision_entries = matrix[lower.tri(matrix, FALSE)]
  calculated_entries = prox_sorted_L1(abs(precision_entries), lambda, method = c("c"))

  out[lower.tri(out, FALSE)] = calculated_entries
  out = t(out)
  out[lower.tri(out, FALSE)] = calculated_entries

  out
}
