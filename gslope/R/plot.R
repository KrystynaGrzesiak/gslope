#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "geom_tile"
#' @importFrom ggplot2 "scale_fill_gradient"

#' @title Plot precision matrix
#'
#' @param x an object of class `'gslope'`
#' @param col a character, plot color. Default "black".
#'
#' @return Prints output on the screen
#'
#' @examples
#' bla bla
#' @rdname plot
#' @export

plot <- function(x, col){
  UseMethod('plot')
}

#'
#' @export
#'
plot.gslope = function(x, col="black"){
  precision_matrix = x$precision_matrix
  x = colnames(precision_matrix)
  y = x
  data = expand.grid(X=x, Y=y)
  data$Z = c(precision_matrix)

  ggplot(data, mapping = aes(X, Y, fill= Z)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high=col)
}




