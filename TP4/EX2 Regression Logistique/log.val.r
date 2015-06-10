log.val <- function(beta,Xtst)
{
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  
  Prob <- matrix(0,nrow=n,ncol=p)
  Xtst <- as.matrix(Xtst)
  
  if(length(beta) == p+1)
  {
    orgin <- rep(1,length=n)
    Xtst <- cbind(orgin,Xtst)
  }
  #print(Xtst)
  #print(beta)
  
  for(i in 1:n)
  {
    z <- exp(t(beta) %*% Xtst[i,])
    Prob[i,1] <- z/(1+z)
    Prob[i,2] <- 1/(1+z)
  }
  
  res <- list()
  res$prob <- Prob
  
  return(res)
  
  
  
}