% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot}
\alias{plot}
\alias{plot.gslope}
\title{Plot precision matrix}
\usage{
plot(x, plt, col, ...)

\method{plot}{gslope}(x, plt = "scaled_precision", col = "black", ...)
}
\arguments{
\item{x}{an object of class `'gslope'`}

\item{plt}{a plot type. Accepts either \code{'precision'}, \code{'covariance'}, \code{'corr'} or \code{'scaled_precision'}. Default \code{'precision'}.}

\item{col}{a character, color name. Default "black"}

\item{...}{Other parameters}
}
\value{
Prints output on the screen
}
\description{
Plot precision matrix
}
\examples{
w = gslope(as.matrix(mtcars))
plot(w, col = "green", plt = "precision")
plot(w, plt = "corr")
plot(w,  col = "purple", plt = "scaled_precision")
}
