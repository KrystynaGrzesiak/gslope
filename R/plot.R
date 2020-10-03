
#' @title Plot precision matrix
#'
#' @param x an object of class `'gslope'`
#' @param col a character, color name. Default "black"
#' @param plt a plot type. Accepts either \code{'precision'}, \code{'covariance'}, \code{'corr'} or \code{'scaled_precision'}. Default \code{'precision'}.
#' @param ... Other parameters
#' @return Prints output on the screen
#'
#' @examples
#' w = gslope(as.matrix(mtcars))
#' plot(w, col = "green", plt = "precision")
#' plot(w, plt = "corr")
#' plot(w,  col = "purple", plt = "scaled_precision")

#' @export

plot <- function(x, plt, col, ...){
  UseMethod('plot')
}

#' @rdname plot
#' @param ... Other parameters
#' @export
#'
plot.gslope = function(x, plt = "scaled_precision", col = "black", ...){
  Y = value = NULL

  if(!(plt %in% c("precision", "corr", "scaled_precision", "covariance"))) stop("Plt must be either precision, covariance, corr or scaled_precision")

  if(plt == "scaled_precision") {
    scaled_precision = x$scaled_precision_matrix
    X = colnames(scaled_precision)
    y = X
    data = expand.grid(X = X, Y = y)
    data$value = c(scaled_precision)

    result = ggplot(data, mapping = aes(X, Y, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = col, high = "white") +
      ggtitle("Scaled precision matrix") +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(hjust = 0.5))
  }

  if(plt == "precision") {
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
    corr_matrix = cov2cor(x$covariance_matrix)
    corr_matrix[upper.tri(corr_matrix)] = NA
    X = colnames(corr_matrix)
    y = X
    data = expand.grid(X = X, Y = y)
    data$value = c(corr_matrix)
    data = data[!is.na(data[["value"]]), ]

    result = ggplot(data, aes(X, Y, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Correlation") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                       size = 10, hjust = 1))+
      ylab("") +
      xlab("") +
      coord_fixed()
  }

  if(plt == "covariance") {
    covariance = x$covariance_matrix
    X = colnames(covariance)
    y = X
    data = expand.grid(X = X, Y = y)
    data$value = c(covariance)

    result = ggplot(data, mapping = aes(X, Y, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = col) +
      ggtitle("Covariance matrix") +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(hjust = 0.5))
  }

  result
}




