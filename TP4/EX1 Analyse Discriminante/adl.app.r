#Fonction apprentissage pour ADL de 2 classes

adl.app <- function(Xapp,zapp)
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
  sigma1 <- var(Xapp_c1) #var() calculer l'estimateur de variance sans bias, c'est V*
  sigma2 <- var(Xapp_c2)
  sigma <- ((length_c1-1)*sigma1+(length_c2-1)*sigma2)/(n-2)
  
  
  res$sigma1 <- sigma
  res$sigma2 <- sigma
  
  return(res)  
} 