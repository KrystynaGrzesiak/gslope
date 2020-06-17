
#' @title Prints all parameters from gslope
#'
#' @param x an object of class `'gslope'`
#'
#' @return Prints following output on the screen:
#' \itemize{
#'
#' \item \code{call} formula used in function call.
#' \item \code{precision matrix} a sparse precision matrix
#' \item \code{covariance matrix} covariance matrix for the data
#' \item \code{scaled precision matrix} see: ?gslope
#' \item \code{lambda} lambda regularizers used
#' \item \code{iterations} number of iterations
#' \item \code{graph} a graph object from igraph package. For more details see ?igraph.
#' \item \code{clusters} clusters revealing sub-graph structure in the model For more details see ?gslope.
#' }
#' @examples
#' d = gslope(as.matrix(mtcars))
#' summary(d)
#'
#' @export
summary <- function(x){
  UseMethod("summary")
}

#' @rdname summary
#' @export
summary.gslope <- function(x){
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
  cat("---\n\n Number of iterations (ADMM):\n\n")
  print(x$iterations)
  cat("---\n\n Graph:\n\n")
  print(x$graph)
  cat("---\n\n Clusters:\n\n")
  print(x$clusters)
}
