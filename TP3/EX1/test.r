source("fonctions-tp3/ceuc.app.R")
source("fonctions-tp3/ceuc.val.r")
source("fonctions-tp3/kppv.app.r")
source("fonctions-tp3/kppv.val.R")
source("fonctions-tp3/front.ceuc.R")
source("fonctions-tp3/front.kppv.R")

donn <- read.table("./donnees-tp3/Synth1-40.txt", header=F)

X <- donn[,1:2]
z <- donn[,3]
Xapp <- X[c(1:15,21:35),]
zapp <- z[c(1:15,21:35)]
Xtst <- X[c(16:20,36:40),]
ztst <- z[c(16:20,36:40)]

#dist <- ceuc.app(Xapp,zapp)
#class <- ceuc.val(dist,Xtst)
#front.ceuc(ceuc.val,dist,Xapp,zapp)

nppv <- c(3,5,7,9,11,13,15)
K <-kppv.app(Xapp,zapp,Xapp,zapp,nppv)
print(K)
front.kppv(kppv.val, K, Xapp, zapp)