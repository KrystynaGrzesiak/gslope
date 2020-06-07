
prox_matrix = function(matrix_in, lambda) {

  if(!(nrow(matrix_in) == ncol(matrix_in))) stop("Comment") #TODO: Add comment

  out = matrix_in
  precision_entries = matrix_in[lower.tri(matrix_in, FALSE)]
  calculated_entries = suppressWarnings(SLOPE::prox_sorted_L1(abs(precision_entries),
                                                            lambda, method = c("c")))
  out[lower.tri(out, FALSE)] = calculated_entries
  out[upper.tri(out, FALSE)] = calculated_entries
  out
}
