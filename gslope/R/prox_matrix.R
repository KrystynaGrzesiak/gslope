
prox_matrix = function(matrix_in,
                       lambda) {

  stopifnot(nrow(matrix_in) == ncol(matrix_in))

  out = matrix_in

  precision_entries = matrix_in[lower.tri(matrix_in, FALSE)]

  if(is.unsorted(rev(lambda)))
    lambda = sort(lambda, decreasing = TRUE)

  calculated_entries = suppressWarnings(SLOPE::prox_sorted_L1(abs(precision_entries),
                                                            lambda, method = c("c")))
  out[lower.tri(out, FALSE)] = calculated_entries
  out = t(out)
  out[lower.tri(out, FALSE)] = calculated_entries

  out
}
