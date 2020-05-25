source("prox_matrix.R")

ADMM_algorithm= function(sampleCovariance, lambda, penalizeDiagonal, mu, Y, Z, maxIter, epsilon){
  
  for(n in 1:maxIter){
    
    Ctilde <- Y-Z-sampleCovariance/mu
    Ceigen <- eigen(Ctilde, symmetric = TRUE)
    CeigenVal <- Ceigen$val
    CeigenVec <- Ceigen$vec
    Fmu <- 1/2*diag(CeigenVal+sqrt(CeigenVal*CeigenVal+4/mu))
    X <- CeigenVec%*%Fmu%*%t(CeigenVec)
    
    Yold <- Y 
    Y <- matrixOWL1prox(X+Z, lambda/mu, penalizeDiagonal) 
    
    Z <- Z + mu*(X-Y)
    
    primalResidual <- norm(X-Y, type = "F")
    dualResidual   <- norm(mu*(Y-Yold), type = "F")
    
    if(primalResidual < epsilon & dualResidual < epsilon) 
      break
  }
  
  X[abs(X) < epsilon] <- 0
  
  return(X)
}

