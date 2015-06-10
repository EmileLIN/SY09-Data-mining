#Fonction apprentissage pour bayes naif de 2 classes

nba.app <- function(Xapp,zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  Xapp_c1 <- Xapp[which(zapp==1),]
  Xapp_c2 <- Xapp[which(zapp==2),]
  
  res <- list()
  
  #estimer pi(proportion)
  length_c1 <- dim(Xapp_c1)[1]
  length_c2 <- dim(Xapp_c2)[1]
  
  res$p1 <- length_c1 / n
  res$p2 <- length_c2 / n
  
  #estimer esperence
  res$mu1 <- colMeans(Xapp_c1)
  res$mu2 <- colMeans(Xapp_c2)
  
  #estimer variance
  #pour bayes naif, on suppose que toutes les variable sont independante,
  #du coup, on prend que le diag de la matrice de variance
  res$sigma1 <- diag(diag(var(Xapp_c1))) 
  res$sigma2 <- diag(diag(var(Xapp_c2)))
 
  return(res)  
  
  
}