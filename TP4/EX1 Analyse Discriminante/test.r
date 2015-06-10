#Un script pour tester le resultat des ADs

source("./adq.app.r")
source("./adl.app.r")
source("./nba.app.r")
source("./ad.val.r")
source("../fonctions-tp4/separ1.R")
source("../fonctions-tp4//prob.ad.R")


#read data
data <- read.table("../donnees-tp4/Synth1-1000.txt",header = F)
data <- data[c(1:50,501:550),]
X <- data[,1:2]
z <- data[,3]
n <- dim(X)[1]

#seperate learning data set and test data set
separ <- separ1(X,z)
Xapp <- separ$Xapp
zapp <- separ$zapp
Xtst <- separ$Xtst
ztst <- separ$ztst

res_adq <- ad.val(adq.app,Xapp,zapp,Xtst)
prob.ad(adq.app,"adq",Xtst,ztst,0.5)
#print("res proba for adq is ")
#print(res_adq)

res_adl <- ad.val(adl.app,Xapp,zapp,Xtst)
prob.ad(adl.app,"adl",Xtst,ztst,0.5)
#print("res proba for adl is ")
#print(res_adl)

res_nba <- ad.val(nba.app,Xapp,zapp,Xtst)
prob.ad(nba.app,"nba",Xtst,ztst,0.5)
#print("res proba for nba is ")
#print(res_nba)





