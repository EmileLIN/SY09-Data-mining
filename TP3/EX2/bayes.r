source("./function.r")

synth1_40 <- read.table("./donnees-tp3/Synth1-40.txt",header=F)
synth1_100 <- read.table("./donnees-tp3/Synth1-100.txt",header=F)
synth1_500 <- read.table("./donnees-tp3/Synth1-500.txt",header=F)
synth1_1000 <- read.table("./donnees-tp3/Synth1-1000.txt",header=F)

synth2_1000 <- read.table("./donnees-tp3/Synth2-1000.txt",header=F)

#Question 04 frontiere de bayes pour synthn 1
#Synth 1
paras <- list()
paras$pi1 <- 0.5
paras$pi2 <- 0.5
paras$mu1 <- c(-2,1)
paras$mu2 <- c(1,1)
paras$sigma1 <- matrix(c(1,0,0,1),nrow=2,byrow = F)
paras$sigma2 <- matrix(c(1,0,0,1),nrow=2,byrow = F)

#Data_40
frontiere_bayes(synth1_40,paras)

#Data_100
frontiere_bayes(synth1_100,paras)

#Data_500
frontiere_bayes(synth1_500,paras)

#Data_1000
frontiere_bayes(synth1_1000,paras)


#Question 05 taux d'erreur de regle de bayes
#Data_40
error_40 <-proba_error(synth1_40,-0.5)
print("Error 40 is ")
print(error_40)

#Data_100
error_100 <-proba_error(synth1_100,-0.5)
print("Error 100 is ")
print(error_100)

#Data_500
error_500 <-proba_error(synth1_500,-0.5)
print("Error 500 is ")
print(error_500)

#Data_1000
error_1000 <-proba_error(synth1_1000,-0.5)
print("Error 1000 is ")
print(error_1000)

# "Error 40 is "
# 0.1010101
# "Error 100 is "
# 0.04014452
#"Error 500 is "
#0.008012821
#"Error 1000 is "
# 0.004002305



