source("fonctions-tp3/separ1.R")
source("./fonctions-tp3/kppv.app.r")
source("./fonctions-tp3/communfunction.r")

data <- read.table("./donnees-tp3/Synth2-1000.txt",header=F)
X <- data[,1:2]
z <- data[,3]

#Question 01

EMV <- EMV_estimation(X,z)
print("EMV is")
print(EMV)

#Question 02
#Distance euclidien

N <- 20
alpha <- 0.95

res_ceuc <- Estimation_interval_confiance_ceuc(X,z,N,alpha)
print("The result of ceuc is ")
print(res_ceuc)
#moyen_app:0.006306306
#moyen_tst: 0.006586826
#IC_app:0.006304685,0.006307928
#IC_tst:0.006585205,0.006588448



#res_ppv <- estimation_interval_confidence_ppv(X,z,N,alpha)
#print("The result of ppv is ")
#print(res_ppvs)
#moyen_app:0.0047
#moyen_tst: 0.0066
#IC_app:0.004697,0.004703
#IC_tst:0.006597,0.006603

