source("fonctions-tp3/separ1.R")
source("./fonctions-tp3/kppv.app.r")
source("./fonctions-tp3/communfunction.r")

synth1_40 <- read.table("./donnees-tp3/Synth1-40.txt",header=F)
synth1_100 <- read.table("./donnees-tp3/Synth1-100.txt",header=F)
synth1_500 <- read.table("./donnees-tp3/Synth1-500.txt",header=F)
synth1_1000 <- read.table("./donnees-tp3/Synth1-1000.txt",header=F)


#Question01 Estimate the parameters of gaussien mix model

#Synth1-40
X_40 <- synth1_40[,1:2]
z_40 <- synth1_40[,3]
n <- dim(X_40)[1]

EMV_40<-EMV_estimation(X_40,z_40)
#print("EMV 40 is ")
#print(EMV_40)

#Synth1-100
X_100 <- synth1_100[,1:2]
z_100 <- synth1_100[,3]
n <- dim(X_100)[1]

EMV_100<-EMV_estimation(X_100,z_100)
#print("EMV 100 is ")
#print(EMV_100)

#Synth1-500
X_500 <- synth1_500[,1:2]
z_500 <- synth1_500[,3]
n <- dim(X_500)[1]

EMV_500<-EMV_estimation(X_500,z_500)
#print("EMV 500 is ")
#print(EMV_500)

#Synth1-100
X_1000 <- synth1_1000[,1:2]
z_1000 <- synth1_1000[,3]
n <- dim(X_1000)[1]

EMV_1000<-EMV_estimation(X_1000,z_1000)
#print("EMV 1000 is ")
#print(EMV_1000)


if(FALSE)
{
#Question 02 

N <- 20
alpha <- 0.95

#Synth1-40
res_40 <- Estimation_interval_confiance_ceuc(X_40,z_40,N,alpha)
print("The result of 40 is ")
print(res_40)

#Synth1-100
res_100 <-Estimation_interval_confiance_ceuc(X_100,z_100,N,alpha)
print("The result of 100 is ")
print(res_100)

#Synth1-500
res_500 <- Estimation_interval_confiance_ceuc(X_500,z_500,N,alpha)
print("The result of 500 is ")
print(res_500)

#Synth1-1000
res_1000 <- Estimation_interval_confiance_ceuc(X_1000,z_1000,N,alpha)
print("The result of 1000 is ")
print(res_1000)



#Question 03
nppv <- c(3,5,7,9,11,13,15)
for(i in 1:10)
{
  K_best_40 <- find_best_K(X_40,z_40,nppv)
  print("K_best is ")
  print(K_best_40)
}
#the most frequent k is 3
}

#Question04
N <- 20
alpha <- 0.95

#Synth1-40
if(FALSE)
{
res_40_ppv <- estimation_interval_confidence_ppv(X_40,z_40,N,alpha)
print("The ppv result of 40 is ")
print(res_40_ppv)
#moyen_app: 0.08
#moyen_tst: 0.13
#IC_app:0.07822647, 0.08177353
#IC_tst:0.1282265,0.1317735


#Synth1-100
res_100_ppv <- estimation_interval_confidence_ppv(X_100,z_100,N,alpha)
print("The ppv result of 100 is ")
print(res_100_ppv)
#moyen_app: 0.081
#moyen_tst: 0.11
#IC_app:0.08061623,0.08138377
#IC_tst:0.1096162,0.1103838


res_500_ppv <- estimation_interval_confidence_ppv(X_500,z_500,N,alpha)
print("The ppv result of 500 is ")
print(res_500_ppv)
#moyen_app: 0.0694
#moyen_tst: 0.0844
#IC_app:0.0693309,0.06946091
#IC_tst:0.08433909,0.08446091



res_1000_ppv <- estimation_interval_confidence_ppv(X_1000,z_1000,N,alpha)
print("The ppv result of 1000 is ")
print(res_1000_ppv)
#moyen_app: 0.0557
#moyen_tst: 0.0736
#IC_app:0.05567157,0.005572843
#IC_tst:0.07357157,0.07362843
}



