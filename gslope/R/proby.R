source("gSlope_admm.R")
library(mvtnorm)


X = rmvnorm(8, rep(0, 10))
alpha = 0.05

gSlope = gSlope_ADMM(X, 
                    lambda = NULL, 
                    penalizeDiagonal = FALSE,
                    mu = 1.1, 
                    Y = NULL,
                    maxIter = 1e5, 
                    epsilon = 1e-4)

gSlope[["precision_matrix"]]
