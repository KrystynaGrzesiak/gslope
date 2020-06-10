#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "geom_tile"
#' @importFrom ggplot2 "scale_fill_gradient"
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "ggtitle"
#' @importFrom ggplot2 "xlab"
#' @importFrom ggplot2 "ylab"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "labs"
#' @importFrom ggplot2 "element_text"
#' @importFrom GGally "ggcorr"

#' @title Plot precision matrix
#'
#' @param x an object of class `'gslope'`
#' @param col a character, color name. Default "black"
#' @param plt a plot type. Accepts either \code{'precision'}, \code{'corr'} or \code{'scaled_precision'}. Default \code{'precision'}.
#'
#' @return Prints output on the screen
#'
#' @examples
#' w = gslope(as.matrix(mtcars))
#' plot(w, col = "green", plt = "precision")
#' plot(w, plt = "corr")
#' plot(w,  col = "purple", plt = "scaled_precision")

#' @export

plot <- function(x, col){
  UseMethod('plot')
}

#' @rdname plot
#' @export
#'
plot = function(x, plt = "precision", col = "black"){
  
  if(!(plt %in% c("precision", "corr", "scaled_precision"))) stop("Plt must be either precision, corr or scaled_precision")

  if(plt == "precision") {
    print("halo")
    precision_matrix = x$precision_matrix
    X = colnames(precision_matrix)
    y = X
    data = expand.grid(X = X, Y = y)
    data$value = c(precision_matrix)
    
    result = ggplot(data, mapping = aes(X, Y, fill = value)) + 
      geom_tile() +
      scale_fill_gradient(low = "white", high = col) +
      ggtitle("Precision matrix") +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  if(plt == "corr") {
    result = ggcorr(cov2cor(x$covariance_matrix), 
           nbreaks = 5, 
           low = col, 
           high = col)+
      labs(title = "Correlation matrix") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  if(plt == "scaled_precision") {
    result = ggcorr(x$scaled_precision_matrix, 
           nbreaks = 5, 
           low = col, 
           high = col)+
      labs(title = "Scaled precision matrix") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  result
}




