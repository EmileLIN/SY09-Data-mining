source("../fonctions-tp4/separ1.R")
source("../fonctions-tp4/prob.log.R")
source("../fonctions-tp4/prob.log2.R")
source("./log.app.r")
source("./log.app.quadra.r")
source("./log.val.r")
source("./log.val.quadra.r")
source("./fonction.r")

#Read Data
synth1 <- read.table("../donnees-tp4/Synth1-1000.txt",header = F)
synth2 <- read.table("../donnees-tp4/Synth2-1000.txt",header = F)
synth3 <- read.table("../donnees-tp4/Synth3-1000.txt",header = F)


#Synth1-1000
X <- synth1[,1:2]
z <- synth1[,3]
N <- 20
intr <- T
epsi <- 0.00001


error_synth1_linear <- rep(0,N)
error_synth1_quadra <- rep(0,N)
path_linear <- "./Graphes/Synth1/Linear/"
path_quadra <- "./Graphes/Synth1/Quadra/"

for(k in 1:N)
{
  separ <- separ1(X,z)
  Xapp <- separ$Xapp
  zapp <- separ$zapp
  Xtst <- separ$Xtst
  ztst <- separ$ztst


  res_synth1_linear <- log.app(Xapp,zapp,intr,epsi)
  if(res_synth1_linear != "ERROR")
  {
    pro_linear <- log.val(res_synth1_linear$beta,Xtst)
    error_synth1_linear[k] <- Proba_error(ztst,pro_linear$prob)
    #prob.log(res_synth1_linear$beta,Xtst,ztst,0.5,path_linear,k)
  }
  else
  {
    break;
  }
  
  #res_synth1_quadra <- log.app.quadra(Xapp,zapp,intr,epsi)
  #Xtst_1 <- quandraticker(Xtst,intr)
  #pro_quadra <- log.val.quadra(res_synth1_quadra$beta,Xtst_1,2)
  #error_synth1_quadra[k]<- Proba_error(ztst,pro_quadra$prob)
  #prob.log2(res_synth1_quadra$beta,Xtst,ztst,0.5,path_quadra,k)
  
}
if(res_synth1_linear != "ERROR")
{
  print("average synth1 linear logistic regression error is ")
  print(mean(error_synth1_linear))
}
#0.02612613
#print("average synth1 quadra logistic regression error is ")
#print(mean(error_synth1_quadra))
#0.02627628

if(FALSE)
{
#Synth2-1000
X <- synth2[,1:2]
z <- synth2[,3]
N <- 20
intr <- T
epsi <- 0.00001


error_synth2_linear <- rep(0,N)
error_synth2_quadra <- rep(0,N)
path_linear <- "./Graphes/Synth2/Linear/"
path_quadra <- "./Graphes/Synth2/Quadra/"

for(k in 1:N)
{
  separ <- separ1(X,z)
  Xapp <- separ$Xapp
  zapp <- separ$zapp
  Xtst <- separ$Xtst
  ztst <- separ$ztst
  
  res_synth2_linear <- log.app(Xapp,zapp,intr,epsi)
  pro_linear <- log.val(res_synth2_linear$beta,Xtst)
  error_synth2_linear[k] <- Proba_error(ztst,pro_linear$prob)
  prob.log(res_synth2_linear$beta,Xtst,ztst,0.5,path_linear,k)
  
  res_synth2_quadra <- log.app.quadra(Xapp,zapp,intr,epsi)
  Xtst_1 <- quandraticker(Xtst,intr)
  pro_quadra <- log.val.quadra(res_synth2_quadra$beta,Xtst_1,2)
  error_synth2_quadra[k]<- Proba_error(ztst,pro_quadra$prob)
  prob.log2(res_synth2_quadra$beta,Xtst,ztst,0.5,path_quadra,k)
  
}
print("average synth2 linear logistic regression error is ")
print(mean(error_synth2_linear))
#0.01242515
print("average synth2 quadra logistic regression error is ")
print(mean(error_synth2_quadra))
#0.01332335
}

if(FALSE)
{
#Synth3-1000
X <- synth3[,1:2]
z <- synth3[,3]
N <- 20
intr <- T
epsi <- 0.00001


error_synth3_linear <- rep(0,N)
error_synth3_quadra <- rep(0,N)
path_linear <- "./Graphes/Synth3/Linear/"
path_quadra <- "./Graphes/Synth3/Quadra/"

for(k in 1:N)
{
  separ <- separ1(X,z)
  Xapp <- separ$Xapp
  zapp <- separ$zapp
  Xtst <- separ$Xtst
  ztst <- separ$ztst
  
  res_synth3_linear <- log.app(Xapp,zapp,intr,epsi)
  pro_linear <- log.val(res_synth3_linear$beta,Xtst)
  error_synth3_linear[k] <- Proba_error(ztst,pro_linear$prob)
  prob.log(res_synth3_linear$beta,Xtst,ztst,0.5,path_linear,k)
  
  res_synth3_quadra <- log.app.quadra(Xapp,zapp,intr,epsi)
  Xtst_1 <- quandraticker(Xtst,intr)
  pro_quadra <- log.val.quadra(res_synth3_quadra$beta,Xtst_1,2)
  error_synth3_quadra[k]<- Proba_error(ztst,pro_quadra$prob)
  prob.log2(res_synth3_quadra$beta,Xtst,ztst,0.5,path_quadra,k)
  
}
print("average synth3 linear logistic regression error is ")
print(mean(error_synth3_linear))
#0.02117117
print("average synth3 quadra logistic regression error is ")
print(mean(error_synth3_quadra))
#0.01576577
}
