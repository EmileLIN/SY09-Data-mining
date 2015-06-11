source("../fonctions-tp4/separ1.R")
source("../EX1 Analyse Discriminante/adq.app.r")
source('../EX1 Analyse Discriminante/adl.app.r')
source('../EX1 Analyse Discriminante/nba.app.r')
source("../EX1 Analyse Discriminante/function.r")
source("../EX1 Analyse Discriminante/ad.val.r")
source("../EX2 Regression Logistique/log.app.r")
source("../EX2 Regression Logistique/fonction.r")
source("../EX2 Regression Logistique/log.val.r")
source("../fonctions-tp4/prob.ad_orginal.R")
source("../fonctions-tp4/prob.log_original.R")
source("../fonctions-tp4/prob.log2_orginal.R")

#read data
Donn <- read.csv("../donnees-tp4/bcw.csv", header=T)
X <- Donn[,1:9]
z <- Donn[,10]
N <- 100
ACP <- T
intr <- T
epsi <- 0.00001
index <- round(runif(1,1,100))

if(ACP == T)
{
  #ACP
  Comp_X <- princomp(X)
  X <- Comp_X$scores
  X_acp <- Comp_X$scores[,1:2]
  png("./Graphes/Breast_cancer/acp.png")
  plot(X_acp,col=c("red","green")[z],pch=16,main="Acp sur tous les donnee ")
  dev.off()
}

error_adq <- rep(0,N)
error_adl <- rep(0,N)
error_nba <- rep(0,N)
error_log_classic <- rep(0,N)

for(i in 1:N)
{
  #Separte randomly the data
  separ <- separ1(X,z)
  Xapp <- separ$Xapp
  zapp <- separ$zapp
  Xtst <- separ$Xtst
  ztst <- separ$ztst
  
  #Analyse discriminante quadratique 
  proba_adq <- ad.val(adq.app,Xapp,zapp,Xtst)
  class_adq <- data_classer(proba_adq)
  error_adq[i] <- data_error_calculator(ztst,class_adq)
  
  #Analyse discriminante lineaire 
  proba_adl <- ad.val(adl.app,Xapp,zapp,Xtst)
  class_adl <- data_classer(proba_adl)
  error_adl[i] <- data_error_calculator(ztst,class_adl)
  
  #Analyse discriminante bayes naif
  proba_nba <- ad.val(nba.app,Xapp,zapp,Xtst)
  class_nba <- data_classer(proba_nba)
  error_nba[i] <- data_error_calculator(ztst,class_nba)
  
  #Regression logistique classique
  model_log_classic <- log.app(Xapp,zapp,intr,epsi)
  proba_log_classic <- log.val(model_log_classic$beta,Xtst)
  error_log_classic[i] <- Proba_error(ztst,proba_log_classic$prob)
    
  
  #Graphes
  if(index == i && ACP ==T)
  {
    Xapp <- Xapp[,1:2]
    Xtst <- Xtst[,1:2]
    
    png("./Graphes/Breast_cancer/adq.png")
    prob.ad (adq.app,Xapp,zapp,Xtst,ztst,0.5,"La frontiere de l'analyse discriminante quadratique")
    dev.off()
    
    png("./Graphes/Breast_cancer/adl.png")
    prob.ad (adl.app,Xapp,zapp,Xtst,ztst,0.5,"La frontiere de l'analyse discriminante lineaire")
    dev.off()
    
    png("./Graphes/Breast_cancer/nba.png")
    prob.ad (nba.app,Xapp,zapp,Xtst,ztst,0.5,"La frontiere de l'analyse discriminante bayse naif")
    dev.off()
    
    param <- log.app(Xapp,zapp,intr,epsi)$beta
    png("./Graphes/Breast_cancer/log_classic.png")
    prob.log(param,Xtst,ztst,0.5,"La frontiere de la regression logistique classique")
    dev.off()
    
  }
}

error_adq_moyen <- mean(error_adq)
print("The Average adq error probability is ")
print(error_adq_moyen)

error_adl_moyen <- mean(error_adl)
print("The Average adl error probability is ")
print(error_adl_moyen)

error_nba_moyen <- mean(error_nba)
print("The Average nba error probability is ")
print(error_nba_moyen)

error_log_classic_moyen <- mean(error_log_classic)
print("The Average classic logistic regression error is ")
print(error_log_classic_moyen)


