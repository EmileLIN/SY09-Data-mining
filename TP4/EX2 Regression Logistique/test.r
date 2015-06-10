source("./log.app.r")
source("./log.val.r")
source("./fonction.r")
source("../fonctions-tp4/separ1.R")
source("../fonctions-tp4/prob.log.R")
source("./log.app.quadra.r")
source("./log.val.quadra.r")
source("../fonctions-tp4/prob.log2.R")

data <- read.table("../donnees-tp4/Synth1-1000.txt",header = F)
data <- data[c(1:50,501:550),]
X <- data[,1:2]
z <- data[,3]
n <- dim(X)[1]

for(k in 1:20)
{
print("---------------------------------")
separ <- separ1(X,z)
Xapp <- separ$Xapp
zapp <- separ$zapp
Xtst <- separ$Xtst
ztst <- separ$ztst

c<- paste("This is the ",k," th times",sep = "")
print(c)
intr <- T
ma <- cbind(Xapp,zapp)
print("the matrix is ")
print(ma)

#Logistic Regression

res <- log.app(Xapp,zapp,intr,0.00001)
#print(res)

#pro <- log.val(res$beta,Xtst)
#print("prob is")
#print(prob)

#error <- Proba_error(ztst,pro$prob)
#print("error is")
#print(error)

#decision_boundary(Xtst,ztst,res$beta)
if(res != "ERROR")
{
  #prob.log(res$beta,Xtst,ztst,0.5)
}
print("---------------------")


if(FALSE)
{
#Logistic Quadratique Regression
res_quadra <- log.app.quadra(Xapp,zapp,intr,0.00001)
#print(res_quadra)

#Xtst <- quandraticker(Xtst,intr)
#pro <- log.val.quadra(res_quadra$beta,Xtst,2)
#print("prob is")
#print(pro)

#error <- Proba_error(ztst,pro$prob)
#print("error is")
#print(error)

prob.log2(res_quadra$beta,Xtst,ztst,0.5)
}

}
