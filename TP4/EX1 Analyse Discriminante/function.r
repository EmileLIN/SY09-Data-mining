source("../fonctions-tp4/separ1.R")
source("./ad.val.r")
source("../fonctions-tp4/prob.ad.R")

data_separtor <- function(synth)
{
  X <- synth[,1:2]
  z <- synth[,3]
  n <- dim(X)[1]
  
  separ <- separ1(X,z)
  
  return (separ)
}

data_analyser <- function(separ,model,name,index)
{
  Xapp <- separ$Xapp
  zapp <- separ$zapp
  Xtst <- separ$Xtst
  ztst <- separ$ztst

  proba <- ad.val(model,Xapp,zapp,Xtst)
  class <- data_classer(proba)
  proba_error <- data_error_calculator(ztst,class)
  
  prob.ad(model,name,Xtst,ztst,0.5)
  
  
  return (proba_error)

}

data_classer <- function(proba)
{
  n <- dim(proba)[1]
  class <- rep(0,length = n)
  
  for(i in 1:n)
  {
    if(proba[i,1] > proba[i,2])
    {
      class[i] = 1
    }
    else{
      class[i] = 2
    }
  }
  return (class)
}

data_error_calculator<-function(Ztst,class)
{
  n <- length(Ztst)
  nb_error <- 0
  
  for(i in 1:n)
  {
    if(Ztst[i] != class[i])
    {
      nb_error <- nb_error +1
    }
  }
  
  res <- nb_error / n
  return (res)
}

