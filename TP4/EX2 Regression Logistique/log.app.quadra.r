#Fonction d'apprentissage du Logistique

log.app.quadra <- function(Xapp,zapp,intr,epsi)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  nb_para <- p*(p+3)/2
  
  Xapp <- as.matrix(Xapp)
  
  #initialiser des variable
  
  x1x2 <- rep(0,n)
  x1x1 <- rep(0,n)
  x2x2 <- rep(0,n)
  for(i in 1:n)
  {
    x1x2[i] <- Xapp[i,1]*Xapp[i,2]
    x1x1[i] <- Xapp[i,1]*Xapp[i,1]
    x2x2[i] <- Xapp[i,2]*Xapp[i,2]
  }
  
  Xapp <-cbind(Xapp,x1x2)
  Xapp <-cbind(Xapp,x1x1)
  Xapp <-cbind(Xapp,x2x2)
  
  
  if(intr == T)
  {
    beta <- rep(0,nb_para+1)
    
    orgin <- rep(1,length=n)
    Xapp <- cbind(orgin,Xapp)
  }
  else
  {
    beta <- rep(0,nb_para)
    
  }
  
  niter <- 0
  
  T <- rep(0,length =n)
  for(i in 1:n)
  {
    if(zapp[i] == 1)
    {
      T[i] = 1
    }
    else
    {
      T[i] = 0
    }
  }
  
  P <- rep(0,length= n)
  for(i in 1:n)
  {
    z <- exp(t(beta) %*% Xapp[i,])
    P[i] <- z / (1+z)
  }
  
  beta_new <- beta
  h <- rep(1,n)     
  #fonction Newton-Rephson
  
  repeat
  {
    beta_old <- beta_new
    
    W <- diag((P*(h-P)))
    
    beta_new <- beta_old + solve((t(Xapp) %*% W %*% Xapp)) %*% t(Xapp) %*% (T-P)
 
  
    #print("beta_new is")
    #print(beta_new)
    
    for(i in 1:n)
    {
      z <- exp(t(beta_new) %*% Xapp[i,])
      P[i] <- z / (1+z)
    }
 
  
  
    niter <- niter +1
    #print("niter is ")
    #print(niter)
    
    if(abs(norm(as.matrix(beta_old))-norm(as.matrix(beta_new))) < epsi || niter >=18)
    {
      break;
    }
  }
  
  beta <- beta_new
  
  LogL <- 0
  for(i in 1:n)
  {
    h <- t(beta) %*% Xapp[i,]
    incre <- T[i] * h  -log(1+exp(1+h))
    LogL <- LogL + incre
  }
  
  res <- list()
  
  res$beta <- beta
  res$niter <- niter
  res$logL <- LogL
  
  return (res)
}