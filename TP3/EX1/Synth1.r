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

#EMV_40<-EMV_estimation(X_40,z_40)
#print("EMV 40 is ")
#print(EMV_40)
#pi:0.55,0.45
#mu1:-1.904681,0.698841
#mu2:0.8808594,0.8587835
#$sigma1
#V1        V2
#V1 0.7701334 0.2566214
#V2 0.2566214 1.0798685

#$sigma2
#V1          V2
#V1  1.41546550 -0.01011499
#V2 -0.01011499  1.22191696

#Synth1-100
X_100 <- synth1_100[,1:2]
z_100 <- synth1_100[,3]
n <- dim(X_100)[1]

#EMV_100<-EMV_estimation(X_100,z_100)
#print("EMV 100 is ")
#print(EMV_100)

#[1] "EMV 100 is "
#$pi1
#[1] 0.53

#$pi2
#[1] 0.47

#$mu1
#V1        V2 
#-1.808233  1.057774 

#$mu2
#V1        V2 
#0.9831412 1.1610775 

#$sigma1
#V1         V2
#V1 1.50880197 0.07097749
#V2 0.07097749 0.95833356

#$sigma2
#V1         V2
#V1 0.84677155 0.04545945
#V2 0.04545945 0.71631862



#Synth1-500
X_500 <- synth1_500[,1:2]
z_500 <- synth1_500[,3]
n <- dim(X_500)[1]

#EMV_500<-EMV_estimation(X_500,z_500)
#print("EMV 500 is ")
#print(EMV_500)

#$pi1
#[1] 0.48

#$pi2
#[1] 0.52

#$mu1
#V1         V2 
#-1.9101695  0.9359349 

#$mu2
#V1        V2 
#0.9979001 0.9843533 

#$sigma1
#V1         V2
#V1 1.16050554 0.03149239
#V2 0.03149239 0.87727376

#$sigma2
#V1          V2
#V1  0.91978725 -0.03484383
#V2 -0.03484383  0.95416768

#Synth1-1000
X_1000 <- synth1_1000[,1:2]
z_1000 <- synth1_1000[,3]
n <- dim(X_1000)[1]

#EMV_1000<-EMV_estimation(X_1000,z_1000)
#print("EMV 1000 is ")
#print(EMV_1000)

#$pi1
#[1] 0.488

#$pi2
#[1] 0.512

#$mu1
#V1        V2 
#-1.998992  1.008186 

#$mu2
#V1        V2 
#1.0908900 0.9837324 

#$sigma1
#V1          V2
#V1  1.03969441 -0.07115634
#V2 -0.07115634  0.97107695

#$sigma2
#V1          V2
#V1  1.00246869 -0.02488749
#V2 -0.02488749  1.02228782



#Question 02 

N <- 20
alpha <- 0.95

#Synth1-40
#res_40 <- Estimation_interval_confiance_ceuc(X_40,z_40,N,alpha)
#print("The result of 40 is ")
#print(res_40)

#$moyen_app
#[1] 0.1240741

#$moyen_tst
#[1] 0.1115385

#$IC_app
#[1] 0.1237852 0.1243630

#$IC_tst
#[1] 0.1112496 0.1118274

#Synth1-100
#res_100 <-Estimation_interval_confiance_ceuc(X_100,z_100,N,alpha)
#print("The result of 100 is ")
#print(res_100)

#$moyen_app
#[1] 0.1439394

#$moyen_tst
#[1] 0.1794118

#$IC_app
#[1] 0.1437528 0.1441260

#$IC_tst
#[1] 0.1792252 0.1795984


#Synth1-500
#res_500 <- Estimation_interval_confiance_ceuc(X_500,z_500,N,alpha)
#print("The result of 500 is ")
#print(res_500)

#$moyen_app
#[1] 0.1223724

#$moyen_tst
#[1] 0.1188623

#$IC_app
#[1] 0.1223373 0.1224074

#$IC_tst
#[1] 0.1188272 0.1188973

#Synth1-1000
#res_1000 <- Estimation_interval_confiance_ceuc(X_1000,z_1000,N,alpha)
#print("The result of 1000 is ")
#print(res_1000)

#$moyen_app
#[1] 0.1048799

#$moyen_tst
#[1] 0.1121257

#$IC_app
#[1] 0.1048704 0.1048894

#$IC_tst
#10112[1] 0.1121163 0.1121352

#Question 03
if(FALSE)
{
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

#res_40_ppv <- estimation_interval_confidence_ppv(X_40,z_40,N,alpha)
#print("The ppv result of 40 is ")
#print(res_40_ppv)
#moyen_app: 0.08
#moyen_tst: 0.13
#IC_app:0.07822647, 0.08177353
#IC_tst:0.1282265,0.1317735


#Synth1-100
#res_100_ppv <- estimation_interval_confidence_ppv(X_100,z_100,N,alpha)
#print("The ppv result of 100 is ")
#print(res_100_ppv)
#moyen_app: 0.081
#moyen_tst: 0.11
#IC_app:0.08061623,0.08138377
#IC_tst:0.1096162,0.1103838


#res_500_ppv <- estimation_interval_confidence_ppv(X_500,z_500,N,alpha)
#print("The ppv result of 500 is ")
#print(res_500_ppv)
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




