# Written by Micha³ Makowski

# Soft thresholding functions definitions
# install.packages("SLOPE")

require(SLOPE)

OWL1fastprox <- function(entries, 
                         lambdaSeries)
{
  return(prox_sorted_L1(entries, lambdaSeries, method = c("c")))
}

matrixOWL1prox <- function(matrix, 
                           lambdaSeries, #must be sorted!
                           penalizeDiagonal = FALSE)
{
  out <- matrix
  
  precisionEntries <- matrix[lower.tri(matrix, penalizeDiagonal)]
  
  calculatedEntries <- OWL1fastprox(abs(precisionEntries), lambdaSeries)
  
  out[lower.tri(out, penalizeDiagonal)] <- calculatedEntries
  out <- t(out)
  out[lower.tri(out, penalizeDiagonal)] <- calculatedEntries
  
  return(out)
}


gslopeADMM <- function(sampleCovariance, 
                       lambda = NULL, 
                       penalizeDiagonal = FALSE,
                       mu = 1.1, 
                       Y = NULL,
                       maxIter = 1e5, 
                       absoluteEpsilon = 1e-4, 
                       # relativeEpsilon = 1e-4, 
                       verbose = TRUE)
{
  # Console output
  
  if(verbose) 
  {
    cat("Starting ADMM gsLOPE procedure...")
    progressBar <- txtProgressBar(min = 0, max = 1/absoluteEpsilon, style = 3)
    setTxtProgressBar(progressBar, 0)
  }    
  
  p <- ncol(sampleCovariance)
  
  # Sequence length
  
  entriesNumber <- sum(1:(p-!penalizeDiagonal))
  
  # Lambda preparation
  
  lambda <- sort(lambda, decreasing = T)
  
  if(length(lambda) == p^2)
  {
    if(penalizeDiagonal)
    {
      lambda <- c(lambda[1:p], lambda[seq(from = p+1, to = length(lambda), by = 2)])
    } else
    {
      lambda <- lambda[seq(from = p+1, to = length(lambda), by = 2)]
    }
  } else if(length(lambda) < entriesNumber)
  {
    lambda <- c(lambda, rep(0, times = entriesNumber - length(lambda)))
  } else if(length(lambda) > entriesNumber)
  {
    lambda <- lambda[1:entriesNumber]
  }
  
  # Initialization
  
  Z <- sampleCovariance*0 # Initialize Lagragian to be nothing (seems to work well)
  if(is.null(Y))
    Y <- Z 
  X <- diag(nrow = p)
  
  # ADMM algotithm
  
  for(n in 1:maxIter)
  {
    # Solve sub-problem to solve X
    Ctilde <- Y-Z-sampleCovariance/mu
    Ceigen <- eigen(Ctilde, symmetric = TRUE)
    CeigenVal <- Ceigen$val
    CeigenVec <- Ceigen$vec
    Fmu <- 1/2*diag(CeigenVal+sqrt(CeigenVal*CeigenVal+4/mu))
    X <- CeigenVec%*%Fmu%*%t(CeigenVec)
    
    # Solve sub-problem to solve Y
    Yold <- Y 
    # Y <- softThresholding(X+Z, lambda/mu) 
    Y <- matrixOWL1prox(X+Z, lambda/mu, penalizeDiagonal) 
    
    # Update the Lagrangian
    Z <- Z + mu*(X-Y)
    
    # Residuals
    primalResidual <- norm(X-Y, type = "F")
    dualResidual   <- norm(mu*(Y-Yold), type = "F")
    
    # Stopping criteria
    primalEpsilon <- absoluteEpsilon # + relativeEpsilon*max(l2norm(X), l2norm(Y))
    # dualEpsilon   <- absoluteEpsilon # + relativeEpsilon*l2norm(Z)
    
    if(verbose)
      setTxtProgressBar(progressBar, min(1/primalResidual, 1/dualResidual, 1/absoluteEpsilon))
    
    if(primalResidual < primalEpsilon & dualResidual < primalEpsilon) 
      break
  }
  
  X[abs(X) < absoluteEpsilon] <- 0
  
  if(verbose) 
    close(progressBar)
  
  return(list(sampleCovariance = sampleCovariance,
              lambda = lambda, 
              lagrangianParameter = mu,
              diagonalPenalization = penalizeDiagonal,
              precisionMatrix = X, 
              covarianceMatrix = solve(X), 
              residuals = c(primalResidual, dualResidual), 
              iterations = n, 
              epsilon = absoluteEpsilon))
}


lambdaSelector <- function(input, n, alpha = 0.05, method = "banerjee", verbose = TRUE)
{
  p <- ncol(input)
  
  if(alpha != 0)
  {
    if(!is.matrix(input))
    {
      if(verbose) cat("The input is identified as a dimension.\n")
      if(!is.numeric(input[1])) stop(paste("The input must be numeric, but is", typeof(input[1])))
      
      p <- input[1]
      twoLargestProd <- 1
      
    } else if(!isSymmetric(input))
    {
      if(verbose) cat("The input is identified as the data matrix.\n")
      
      n <- nrow(input)
      input <- cov(scale(input))
      twoLargestProd <- 1
    } else
    { 
      if(verbose) cat("The input is identified as the covariance matrix.\n")
      
      twoLargestProd <- prod(-sort(-diag(input), partial = 2)[1:2]) # In case data wasn't scaled
    }
    
    out <- switch(method,
                  glasso = lambdaGLASSO(p, n, alpha, twoLargestProd),
                  banerjee = lambdaBanerjee(p, n, alpha, twoLargestProd),
                  BH = lambdaBH(p, n, alpha, twoLargestProd),
                  holm = lambdaHolm(p, n, alpha, twoLargestProd))
  } else
  {
    out <- 0
  }
  
  return(out)
}


# BH for SLOPE (not sure if done properly - problem with sigma i.e. twoLargestProd if data not scaled)
lambdaBH <- function(p, n, alpha = 0.05, twoLargestProd = 1)
{
  pBH <- p*(p-1)/2
  k <- 1:pBH
  fractionSeq <- qt(1-alpha*k/2/pBH, df = n-2)/sqrt(n-2+qt(1-alpha*k/2/pBH, df = n-2)^2)
  fractionSeq <- c(rep(fractionSeq[1], p), rep(fractionSeq, each=2))
  
  return(twoLargestProd*fractionSeq)
}

# Holm for SLOPE (not sure if done properly - problem with sigma i.e. twoLargestProd if data not scaled)
lambdaHolm <- function(p, n, alpha = 0.05, twoLargestProd = 1)
{
  pHolm <- p*(p-1)/2
  k <- 1:pHolm
  fractionSeq <- qt(1-alpha/2/(pHolm+1-k), df = n-2)/sqrt(n-2+qt(1-alpha/2/(pHolm+1-k), df = n-2)^2)
  fractionSeq <- c(rep(fractionSeq[1], p), rep(fractionSeq, each=2))
  
  return(twoLargestProd*fractionSeq)
}

X = rmvnorm(8, rep(0, 10))
p = ncol(X)
n = nrow(X)
alpha = 0.5
sampleCovariance = cov(X)
epsilon = 10e-4 
mu = 1


BHSlopeLambda <- lambdaSelector(p, n, alpha, "BH", verbose = FALSE)
# gSLOPE
gSlopeADMM <- gslopeADMM(sampleCovariance, mu = mu, lambda = BHSlopeLambda, 
                         absoluteEpsilon = epsilon)
gSlopeADMM

View(gSlopeADMM[["precisionMatrix"]])