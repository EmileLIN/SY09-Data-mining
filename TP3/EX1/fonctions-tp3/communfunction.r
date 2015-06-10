source("fonctions-tp3/separ1.R")
source("fonctions-tp3//separ2.R")
source("fonctions-tp3//ceuc.app.R")
source("fonctions-tp3/ceuc.val.r")
source("fonctions-tp3/kppv.app.r")
source("fonctions-tp3//kppv.val.R")
source("fonctions-tp3//front.ceuc.R")

library(MASS)

#For two group
EMV_estimation <- function(X,z)
{
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  pi1 <- 0
  pi2 <- 0
  mu1 <- rep(0,length=p)
  mu2 <- rep(0,length=p)
  sigma1 <- matrix(0,nrow=p,ncol=p)
  sigma2 <- matrix(0,nrow=p,ncol=p)
  
  X_class1 <- X[which(z == 1),]
  X_class2 <- X[which(z == 2),]
  

  print(c)
  
  n1 <- dim(X_class1)[1]
  n2 <- dim(X_class2)[1]
  
  #calculate pi
  pi1 <- n1 / n
  pi2 <- n2 / n
  
  #calculate mu
  mu1<-colMeans(X_class1)
  mu2<-colMeans(X_class2)
  
  #calculate sigma
  sigma1<-var(X_class1)
  sigma2<-var(X_class2)
 
  res <- list()
  res$pi1 <- pi1
  res$pi2 <- pi2
  res$mu1 <- mu1
  res$mu2 <- mu2
  res$sigma1 <- sigma1
  res$sigma2 <- sigma2
  
  return (res)
   
}

norm_interval_confidence<-function(data,moyen,var,level)
{
  n <- length(data)
  
  z <- qnorm((1+level)/2,mean=0,sd=1)
 
  IC<- c(moyen-z*sqrt(var),moyen+z*sqrt(var))
  
  return (IC)
  
  
}

Estimation_interval_confiance_ceuc <- function(X,z,N,alpha)
{
  m <- dim(X)[1]
  Error_app <- rep(0,length=N)
  Error_tst <- rep(0,length=N)
  
  for(i in 1:N)
  {
    data_sep <- separ1(X,z)
    Xapp <- data_sep$Xapp
    zapp <- data_sep$zapp
    Xtst <- data_sep$Xtst
    ztst <- data_sep$ztst
    
    nb_app <- dim(Xapp)[1]
    nb_tst <- dim(Xtst)[1]
    
    mu <- ceuc.app(Xapp,zapp)
    class_app <- ceuc.val(mu,Xapp)
    class_tst <- ceuc.val(mu,Xtst)
    
    Error_app[i]<- Error_proba(class_app,zapp,length(zapp))
    Error_tst[i]<- Error_proba(class_tst,ztst,length(ztst))
    
    name <- paste(c("graphe_ceuc",i,".png"))
          
  
    
  }
  
  moyen_Error_app <- mean(Error_app)
  moyen_Error_tst <- mean(Error_tst)
 
  var_Error_app <- moyen_Error_app*(1-moyen_Error_app)/(nb_app*N)
  var_Error_tst <- moyen_Error_tst*(1-moyen_Error_tst)/(nb_tst*N)
  
  IC_app <-norm_interval_confidence(Error_app,moyen_Error_app,var_Error_app,alpha)
  IC_tst <-norm_interval_confidence(Error_tst,moyen_Error_tst,var_Error_tst,alpha)
  
  res <- list()
  
  res$error_app <- Error_app
  res$error_tst <- Error_tst
  res$moyen_app <-moyen_Error_app
  res$moyen_tst <-moyen_Error_tst
  res$IC_app <- IC_app
  res$IC_tst <- IC_tst
  
  return (res)
}


find_best_K <- function(X,z,nppv)
{
  data_sep <- separ1(X,z)
  Xapp <- data_sep$Xapp
  zapp <- data_sep$zapp
  Xtst <- data_sep$Xtst
  ztst <- data_sep$ztst
  
  K_best <- kppv.app(Xapp,zapp,Xapp,zapp,nppv)
  
  return(K_best)
}

estimation_interval_confidence_ppv <- function(X,z,N,alpha)
{
  m <- dim(X)[1]
  Error_app <- rep(0,length=N)
  Error_tst <- rep(0,length=N)
  nppv <- c(3,5,7,9,11,13,15)
  
  for(i in 1:N)
  {
    data_sep <- separ2(X,z)
    Xapp <- data_sep$Xapp
    zapp <- data_sep$zapp
    Xval <- data_sep$Xval
    zval <- data_sep$zval
    Xtst <- data_sep$Xtst
    ztst <- data_sep$ztst
    
    nb_app <- dim(Xapp)[1]
    nb_tst <- dim(Xtst)[1]
        
    K <- kppv.app(Xapp,zapp,Xval,zval,nppv)
    class_app <- kppv.val(Xapp,zapp,K,Xapp)
    class_tst <- kppv.val(Xapp,zapp,K,Xtst)
    
    Error_app[i]<- Error_proba(class_app,zapp,length(zapp))
    Error_tst[i]<- Error_proba(class_tst,ztst,length(ztst))
  }
  
  moyen_Error_app <- mean(Error_app)
  moyen_Error_tst <- mean(Error_tst)
  var_Error_app <- moyen_Error_app*(1-moyen_Error_app)/(nb_app*N)
  var_Error_tst <- moyen_Error_tst*(1-moyen_Error_tst)/(nb_tst*N)
  
  IC_app <-norm_interval_confidence(Error_app,moyen_Error_app,var_Error_app,alpha)
  IC_tst <-norm_interval_confidence(Error_tst,moyen_Error_tst,var_Error_tst,alpha)
  
  res <- list()
  res$error_app <- Error_app
  res$error_tst <- Error_tst
  res$moyen_app <-moyen_Error_app
  res$moyen_tst <-moyen_Error_tst
  res$IC_app <- IC_app
  res$IC_tst <- IC_tst
  
  return (res)
  
}



