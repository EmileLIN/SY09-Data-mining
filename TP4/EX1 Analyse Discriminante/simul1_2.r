source("./function.r")


#read_data
synth1 <- read.table("../donnees-tp4/Synth1-1000.txt",header = F)
synth2 <- read.table("../donnees-tp4/Synth2-1000.txt",header = F)
synth3 <- read.table("../donnees-tp4/Synth3-1000.txt",header = F)

N <- 20

#Synth1-1000

error_synth1_adq <- rep(0,length=N)
error_synth1_adl <- rep(0,length=N)
error_synth1_nba <- rep(0,length=N)
for(i in 1:20)
{
  separ <- data_separtor(synth1)
  
  #ADQ
  name<- list()
  name$path <- paste("./Graphes/Synth1-1000/","adq/",sep = '')
  name$filename <- paste("EX1_Q2_adq_",i,sep = '')
  name$modelname <- "adq"
  error_synth1_adq[i] <- data_analyser(separ,adq.app,name)
  
  #ADL
  name<- list()
  name$path <- paste("./Graphes/Synth1-1000/","adl/",sep = '')
  name$filename <- paste("EX1_Q2_adl_",i,sep = '')
  name$modelname <- "adl"
  error_synth1_adl[i] <- data_analyser(separ,adl.app,name)
  
  #NBA
  name<- list()
  name$path <- paste("./Graphes/Synth1-1000/","nba/",sep = '')
  name$filename <- paste("EX1_Q2_nba_",i,sep = '')
  name$modelname <- "nba"
  error_synth1_nba[i] <- data_analyser(separ,nba.app,name)
}
#ADQ
error_synth1_adq_moyen <- mean(error_synth1_adq)
print("error synth1 adq moyen is ")
print(error_synth1_adq_moyen)

#ADL
error_synth1_adl_moyen <- mean(error_synth1_adl)
print("error synth1 adl moyen is ")
print(error_synth1_adl_moyen)

#NBA
error_synth1_nba_moyen <- mean(error_synth1_nba)
print("error synth1 nba moyen is ")
print(error_synth1_nba_moyen)





#Synth2-1000

error_synth2_adq <- rep(0,length=N)
error_synth2_adl <- rep(0,length=N)
error_synth2_nba <- rep(0,length=N)

for(i in 1:20)
{
    
  separ <- data_separtor(synth2)

  #ADQ

  name<- list()
  name$path <- paste("./Graphes/Synth2-1000/","adq/",sep = '')
  name$filename <- paste("EX1_Q2_adq_",i,sep = '')
  name$modelname <- "adq"
  error_synth2_adq[i] <- data_analyser(separ,adq.app,name)
  
  #ADL
  name<- list()
  name$path <- paste("./Graphes/Synth2-1000/","adl/",sep = '')
  name$filename <- paste("EX1_Q2_adl_",i,sep = '')
  name$modelname <- "adl"
  error_synth2_adl[i] <- data_analyser(separ,adl.app,name)
  
  #NBA
  name<- list()
  name$path <- paste("./Graphes/Synth2-1000/","nba/",sep = '')
  name$filename <- paste("EX1_Q2_nba_",i,sep = '')
  name$modelname <- "nba"
  error_synth2_nba[i] <- data_analyser(separ,nba.app,name)
  
}

#ADQ
error_synth2_adq_moyen <- mean(error_synth2_adq)
print("error synth2 adq moyen is ")
print(error_synth2_adq_moyen)

#ADL
error_synth2_adl_moyen <- mean(error_synth2_adl)
print("error synth2 adl moyen is ")
print(error_synth2_adl_moyen)

#NBA
error_synth2_nba_moyen <- mean(error_synth2_nba)
print("error synth2 nba moyen is ")
print(error_synth2_nba_moyen)


#Synth3-1000

error_synth3_adq <- rep(0,length=N)
error_synth3_adl <- rep(0,length=N)
error_synth3_nba <- rep(0,length=N)

for(i in 1:20)
{
  
  separ <- data_separtor(synth3)
  
  #ADQ
  
  name<- list()
  name$path <- paste("./Graphes/Synth3-1000/","adq/",sep = '')
  name$filename <- paste("EX1_Q2_adq_",i,sep = '')
  name$modelname <- "adq"
  error_synth3_adq[i] <- data_analyser(separ,adq.app,name)
  
  #ADL
  name<- list()
  name$path <- paste("./Graphes/Synth3-1000/","adl/",sep = '')
  name$filename <- paste("EX1_Q2_adl_",i,sep = '')
  name$modelname <- "adl"
  error_synth3_adl[i] <- data_analyser(separ,adl.app,name)
  
  #NBA
  name<- list()
  name$path <- paste("./Graphes/Synth3-1000/","nba/",sep = '')
  name$filename <- paste("EX1_Q2_nba_",i,sep = '')
  name$modelname <- "nba"
  error_synth3_nba[i] <- data_analyser(separ,nba.app,name)
  
}

#ADQ
error_synth3_adq_moyen <- mean(error_synth3_adq)
print("error synth3 adq moyen is ")
print(error_synth3_adq_moyen)

#ADL
error_synth3_adl_moyen <- mean(error_synth3_adl)
print("error synth3 adl moyen is ")
print(error_synth3_adl_moyen)

#NBA
error_synth3_nba_moyen <- mean(error_synth3_nba)
print("error synth3 nba moyen is ")
print(error_synth3_nba_moyen)

