#Classifier of the most close neibour
#for just two classes
class_decision <- function(zapp,index,k)
{
  nb_class1 <- 0
  nb_class2 <- 0

  for(i in index)
  {
    if(zapp[i] == 1) 
    {
      nb_class1 <- nb_class1+1
    }
    else if (zapp[i] == 2)
    {
      nb_class2 <- nb_class2+1
    }
    else
    {
      
    }
  }
  
  if(nb_class1 > nb_class2)
  {
    return (1)
  }
  else{
    return (2)
  }
}

Error_proba <- function(class,zval,nval)
{
  Error_proba <- 0
  nberror <- 0
  
  for(m in 1:nval)
  {
    if(zval[m] != class[m])
    {
      nberror <- nberror+1
    }
  }
  
  Error_proba <- nberror/nval
  return (Error_proba)
}

Find_min_k <- function(Error_proba,nppv)
{
  n <- length(nppv)
  min_k <- 1
  Error_probamin <- Error_proba[1]
 
  
  for(k in nppv)
  {
    if(Error_proba[k]<Error_probamin)
    {
      Error_probamin <- Error_proba[k]
      min_k <- k
    }
  }
  
  return (min_k)
}

indexOf_K_Minimum<- function(Xapp_dist,k)
{
  index <- rep(0,k)
  n <- dim(Xapp_dist)[1]
  indice <- rep(1:n,length = n)
  Xapp_dist <- cbind(indice,Xapp_dist)
  
  Xapp_dist <- Xapp_dist[order(Xapp_dist[,4],decreasing = F),]
  for(i in 1:k)
  {
    index[i] <- Xapp_dist[i,1]
  }
  return (index)
}

kppv.app <- function(Xapp,zapp,Xval,zval,nppv)
{
  napp <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  nval <- dim(Xval)[1]
  g <- unique(zapp)
  nb_g <- length(unique(zapp))
 
  dist <- rep(0,napp)
  Error_proba <- rep(1,max(nppv))

  for(k in nppv)
  {
    class <- rep(0,nval)
    nberror <- 0
   
    for(j in 1:nval)
    {
      for(i in 1:napp)
      {
        dist[i] <- dist(rbind(Xapp[i,],Xval[j,]))
      }
      Xapp_dist <- cbind(Xapp,dist)
  
      index <- indexOf_K_Minimum(Xapp_dist,k)
    
      class[j] <- class_decision(zapp,index,k)
      
    }
 
    Error_proba[k] <- Error_proba(class,zval,nval)
  
  }


  return (Find_min_k(Error_proba,nppv))
}