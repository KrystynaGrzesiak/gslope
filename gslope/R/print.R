#' @title Prints summary from gslope
#'
#' @param x an object of class `'gslope'`
#'
#' @return Prints output on the screen
#'
#' @examples
#' bla bla
#'
#' @export
print <- function(x){
  UseMethod("print")
}

#' @rdname print
#' @export
print.gslope <- function(x){
  cat("Call:\n\n")
  print(x$call)
  cat("---\n\n Sparse precision matrix:\n\n")
  print(x$precision_matrix)
  cat("---\n\n Covariance matrix:\n\n")
  print(x$covariance_matrix)
  cat("---\n\n Scaled sparse precision matrix:\n\n")
  print(x$scaled_precision_matrix)
  cat("---\n\n Lambda regularizers used:\n\n")
  print(x$lambda)
  cat("---\n\n Number of iteration (ADMM):\n\n")
  print(x$iterations)
}


