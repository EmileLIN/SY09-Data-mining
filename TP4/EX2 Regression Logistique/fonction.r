Proba_error <- function(ztst,proba)
{
  n <- dim(proba)[1]

  
  class <- rep(0,n) 
  error <- 0
    
  for(i in 1:n)
  {
    if(proba[i,1] > proba[i,2])
    {
      class[i] <-1
    }
    else
    {
      class[i] <-2
    }
    
    if(class[i] != ztst[i])
    {
      error <- error +1
    }
  }
  
  error <- error/n
  return(error)
}

decision_boundary <- function(Xtst,ztst,beta)
{
  p <- length(beta)
  if(p == 2)
  {
    slope <- -beta[1]/beta[2]
    intercept <- 0
  }
  else
  {
    slope <- -beta[2]/beta[3]
    intercept <- -beta[1]/beta[3]
  }
  
  plot(Xtst,col=ztst,pch=16,main="Decision Boundary of logistic regression")
  abline(a=slope,b=intercept,col=4)
}

LogLCalcul <- function(beta,X,Tlist)
{
  n<- dim(X)[1]
  
  LogL <- 0
  for(i in 1:n)
  {
    h <- t(beta) %*% X[i,]
    incre <- Tlist[i] * h  -log(1+exp(h))
    LogL <- LogL + incre
  }
  
  return (LogL)
}

quandraticker <- function(X,intr)
{
  n <- dim(X)[1]
  
  x1x2 <- rep(0,n)
  x1x1 <- rep(0,n)
  x2x2 <- rep(0,n)
  
  for(i in 1:n)
  {
    x1x2[i] <- X[i,1]*X[i,2]
    x1x1[i] <- X[i,1]*X[i,1]
    x2x2[i] <- X[i,2]*X[i,2]
  }
  
  X <-cbind(X,x1x2)
  X <-cbind(X,x1x1)
  X <-cbind(X,x2x2)
  
  if(intr == T)
  {
    orgin <- rep(1,length=n)
    X <- cbind(orgin,X)
  }
  
  return (X)
}

complete_separation <- function(Xapp,zapp,beta)
{
  n <- dim(Xapp)[1]
  y <- rep(0,n)
  z <- rep(0,n)
  class <- rep(0,n)
  
  for(i in 1:n)
  {
    if(zapp[i] ==1)
    {
      class[i] <- 1  
    }
    else
    {
      class[i] <- 0
    }
    
    z[i] <- t(beta) %*% Xapp[i,]

    y[i] <- 1/(1+exp((2*class[i]-1)*z[i]))
  }
  print("Z is")
  print(z)
  print("y is")
  print(y)
  
}  


  
  

