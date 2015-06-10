frontiere_bayes <- function(data,paras)
{
  X <- data[,1:2]
  z <- data[,3]
  n <- dim(data)[1]
  
  X_class1 <- X[which(z==1),]
  X_class2 <- X[which(z==2),]
  
  pngName <- paste("./graphe/bayes",n,".png",sep = "",collapse = NULL)
  title <- paste("La frontiere de Bayes pour sythn_",n)
  
  #png("./graphe/bayes.png")
  png(pngName)
  plot(X,col=z,pch=16,main=title)
  abline(v=-0.5)
  dev.off()
  
  
}  

proba_error <- function(data,sparator)
{
  X <- data[,1:2]
  z <- data[,3]
  n <- dim(data)[1]
  
  X_class1 <- X[which(z==1),]
  X1_class1 <- X_class1[,1]
  
  
  X_class2 <- X[which(z==2),]
  X1_class2 <- X_class2[,1] 
  
  #calculer proba d'erreur que les points qui sont classe1, mais on le traite comme classe2
  error_21 <- dim(X_class1[which(X1_class1>sparator),])[1]/dim(X_class1)[1]
  
  
  #calculer proba d'erreur que les points qui sont classe2, mais on le traite comme classe1
  error_12 <- dim(X_class2[which(X1_class2<sparator),])[1]/dim(X_class2)[1]
  
  error <- error_21*0.5+error_12*0.5
  
  return (error)  
  
}