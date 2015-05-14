source("fonctions-tp3/separ1.R")
source("./fonctions-tp3/kppv.app.r")
source("./fonctions-tp3/communfunction.r")

data <- read.table("./donnees-tp3/Synth2-1000.txt",header=F)
X_40 <- X[,1:2]
z_40 <- X[,3]

