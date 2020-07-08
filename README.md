# gSLOPE: sparse precision matrix estimation with Sorted L-One Penalizaed Estimation (SLOPE)

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/StatsIMUWr/gslope.svg?branch=master)](https://travis-ci.org/StatsIMUWr/gslope)
<!-- badges: end -->


`gSLOPE` is an R package that estimates a sparse precision matrix based on regularization by the [SLOPE](https://arxiv.org/pdf/1407.3824.pdf).

## Installation

Run the following code in R console to download the latest development version from Github:

```R
if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github("StatsIMUWr/gslope")
```

## Usage

The main function of the package is `gslope`. It estimates the precision matrix . `summary` and `plot` methods are available. Details can be found in [documentation online](https://statsimuwr.github.io/gslope/).

```R
library(gslope)

d = gslope(mtcars)
summary(d)
coef(d)
graph_plot(d)
plot(d, col = "navy", plt = "corr")
plot(d, plt = "precision")
```
