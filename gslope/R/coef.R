#' @title Prints precision matrix from gslope
#'
#' @param x an object of class `'gslope'`
#'
#' @return Prints output on the screen
#'
#' @examples
#' bla bla
#'
#' @export
#'

coef <- function(x){
  UseMethod('coef')
}

#'
#' @export
#'
coef.gslope <- function(x){
  return(x$precision_matrix)
}
