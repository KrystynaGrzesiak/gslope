# gslope

Gslope is an R package that estimates a precision matrix based on ADMM algorithm.

The package contains:

`ADMM_algorithm()`  - ADMM algorithm for graphical SLOPE

`coef()` - Prints precision matrix from gslope

`create_lambda()` - Penalty parameters for graphical SLOPE

`graph_plot()` - Plot graph for gslope.

`gslope()` - Graphical SLOPE

`plot()` - Plot precision matrix

`prepare_lambda()` - Preparation of lambda

`summary()` - Prints all parameters from gslope


## Overview


## Installation

```R
if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github("StatsIMUWr/gslope")
```

## Usage



```R
library(gslope)

d = gslope(mtcars)
summary(d)
coef(d)
graph_plot(d)
plot(d, col = "navy", plt = "corr")
plot(d, plt = "precision")
```
