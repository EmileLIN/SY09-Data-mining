#Fonction d'apprentissage du Logistique

log.app <- function(Xapp,zapp,intr,epsi)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  Xapp <- as.matrix(Xapp)

  
  #initialiser des variable

  if(intr == T)
  {
    beta <- rep(0,p+1)
   
    orgin <- rep(1,length=n)
    Xapp <- cbind(orgin,Xapp)
  }
  else
  {
    beta <- rep(0,p)

  }
  
  niter <- 0
  
  Tlist <- rep(0,length =n)
  for(i in 1:n)
  {
    if(zapp[i] == 1)
    {
      Tlist[i] = 1
    }
    else
    {
      Tlist[i] = 0
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
  LogL <- rep(0,100)
  repeat
  {
    niter <- niter +1
    beta_old <- beta_new
    
    W <- diag((P*(h-P)))
       
    beta_new <- beta_old + solve((t(Xapp) %*% W %*% Xapp)) %*% t(Xapp) %*% (Tlist-P)
    #print("beta_new is")
    #print(beta_new)
    
    LogL[niter] <- LogLCalcul(beta_new,Xapp,Tlist)
       
    for(i in 1:n)
    {
      z <- exp(t(beta_new) %*% Xapp[i,])
      P[i] <- z / (1+z)
    }
 
    #print("niter is ")
    #print(niter)
    
  
    differ <- abs(norm(as.matrix(beta_old))-norm(as.matrix(beta_new)))
    print("differ is ")
    print(differ)
    if(is.nan(differ))
    {
      complete_separation(Xapp,zapp,beta_old)
      print("There is a problem")
      break;
    }
    else
    {
       if(niter >=100 || differ < epsi )
       {
          break;
       }
    }
  }
  
  x <- c(1:niter)
  print("the proba is ")
  print(P)
  print("x is ")
  print(x)
  LogL_final <- rep(0,niter)
  for(i in 1:niter)
  {
    LogL_final[i] <- LogL[i]
  }
  print("LogL is")
  print(LogL_final)
  plot(x,LogL_final,pch=16) 
  
  beta <- beta_new
  
  LogL_final <- LogLCalcul(beta,Xapp,Tlist)
  
  res <- list()
  
  res$beta <- beta
  res$niter <- niter
  res$logL <- LogL_final
  
  return (res)
}