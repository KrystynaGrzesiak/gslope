
#' @importFrom igraph "graph_from_adjacency_matrix"
#' @importFrom igraph "cluster_optimal"

#' @title Prints all parameters from gslope
#'
#' @param x an object of class `'gslope'`
#'
#' @return Prints output on the screen
#'
#' @examples
#' d=gslope(as.matrix(mtcars))
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
  cat("---\n\n Clusters:\n\n")
  precision = x$precision_matrix
  precision[precision != 0] = 1
  graph = graph_from_adjacency_matrix(precision, mode = c("undirected"), weighted = NULL,
                                      diag = TRUE, add.colnames = NULL, add.rownames = NA)
  print(cluster_optimal(graph))
}
