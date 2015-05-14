source("./fonctions-tp3/ceuc.app.R")
source("./fonctions-tp3/ceuc.val.r")
source("./fonctions-tp3/kppv.app.r")
source("./fonctions-tp3/kppv.val.R")
source("./fonctions-tp3/front.ceuc.R")
source("./fonctions-tp3/front.kppv.R")
source("fonctions-tp3/separ1.R")
source("fonctions-tp3/separ2.R")

#Example
data <- read.table("./donnees-tp3/Synth1-500.txt",header=F)
X <- data[,1:2]
z <- data[,3]
n <- dim(X)[1]

Xapp <- X[c(1:round(3*n/8),(round(n/2)+1):round(7*n/8)),]
zapp <- z[c(1:round(3*n/8),(round(n/2)+1):round(7*n/8))]
Xtst <- X[c((round(3*n/8)+1):round(n/2),(round(7*n/8)+1):n),]
ztst <- z[c((round(3*n/8)+1):round(n/2),(round(7*n/8)+1):n)]

#Test for classifer euclidien distance 
mu <- ceuc.app(Xapp,zapp)
class_euc <- ceuc.val(mu,Xtst)
print("The class for euclidien distance classifer is ")
print(class_euc)

tst_len <- dim(Xtst)[1]
Error_proba_euc <- Error_proba(class_euc,ztst,tst_len)
print("The Error probability of euclidien distance is")
print(Error_proba_euc)

#Decision graph
#front.ceuc(ceuc.val,mu,Xapp,zapp)


#Test for classfier the most close neibour
app_length <- dim(Xapp)[1]

data_sep <- separ2(X,z)
Xapp<-data_sep$Xapp
zapp<-data_sep$zapp
Xval<-data_sep$Xval
zval<-data_sep$zval
Xtst<-data_sep$Xtst
ztst<-data_sep$ztst

napp <- c(2,3,4)

K <- kppv.app(Xapp,zapp,Xval,zval,napp)
class_ppv <- kppv.val(Xapp,zapp,K,Xtst)
print("The optimal k is ")
print(K)
print("The class for the most closed neibour classifer is ")
print(class_ppv)

tst_len <- dim(Xtst)[1]
Error_proba_ppv <- Error_proba(class_ppv,ztst,tst_len)
print("The Error probability of ppv is")
print(Error_proba_ppv)

#Decision Front
#front.kppv(kppv.val,K,Xapp,zapp)



