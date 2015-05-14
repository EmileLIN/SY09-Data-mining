# The aim of this exercice is to understand the process AFTD

#Help Function
IngoreNegativeEigenVal <- function(eigenVal){
  n <- length(eigenVal)
  for (i in 1:n)
  {
    if(eigenVal[i]< 0)
    {
      eigenVal[i] <- 0
    }
  }
  return (eigenVal)
}

#Source Data
data <- matrix(c(8.5,1.5,3.5,5.0,2.0,6.5,9.5,1.5,8.5,2.5,3.0,6.5,9.0,2.5,2.0,5.5),nrow=8,byrow=T)
n <- dim(data)[1]

data <- scale(data,center=T,scale =F)

#Question 01 calcul the distance matrix D2 related to data
Dis <- dist(data,method = "euclidean")
Dis <- as.matrix(Dis)
Dis <- Dis^2


#Qustion 02 calcul the covariance matrix from source data and from distance matrix
#From source data
W <- data %*% t(data)

#From distance data
Un <- matrix(1,nrow = n,ncol =n)
Qn <- diag(n) - Un/n

W_dis <- -0.5 * Qn %*% Dis %*% Qn

#The W and W_dis are the same

#Question 03 ,Question 04
#Diagonalize the W
eigen <- eigen(W/n)
B <- eigen$vectors
eigenVal <- eigen$values
#The 7th and 8th eigen values are negative,so this is not SDP

#normalize the matrix B
B <- B * sqrt(n)


#Question 05
#The negative eigen values are very small,so we can ignore them
eigenVal <- IngoreNegativeEigenVal(eigenVal)
L <- diag(eigenVal)

C <- B %*% sqrt(L)

#Question 06 
png("./graphic/Ex1_Q6_reps_src.png")
plot(data,main="Representation by source data",xlab="first factor",ylab="second factor",col="red",pch=16)
dev.off()

png("./graphic/Ex1_Q6_reps_aftd.png")
plot(C[,1],C[,2],main="Representation by aftd components",xlab="first comp",ylab="second comp",col="red",pch=16)
dev.off()

#Question 07 
aftd <- function(Dis)
{
  aftd <- list()
  
  n <- dim(Dis)[1]
  Un <- matrix(1,nrow = n,ncol =n)
  Qn <- diag(n) - Un/n
  
  W <- -0.5 * Qn %*% Dis %*% Qn
  
  eigen <- eigen(W/n)
  B <- eigen$vectors
  eigenVal <- eigen$values
  B <- B * sqrt(n)
  
  eigenVal <- IngoreNegativeEigenVal(eigenVal)
  L <- diag(eigenVal)
  
  C <- B %*% sqrt(L)
  
  aftd$scores <- C
  
  #Calcul the Quality
  quality <- rep(0,length = n)
  total <- sum(eigenVal)
  for (i in 1:n)
  {
    part <- 0
    for (j in 1:i)
    {
      part <- part + eigenVal[j] 
    }
    quality[i] <- part/total
  }
  
  aftd$quality <- quality
  
  return(aftd)   
}

# Questions 1:
mutations <- read.table("mutations2.txt", header=F, row.names=1)
mutations <- as.dist(mutations)
mutations_matrix <- as.matrix(mutations)

# realisation de l'aftd avec la fonction "aftd()" de l'exercice 1
aftd(mutations_matrix)

png("C:/Users/Grégory/Dropbox/SY09/TP2/EX1 AFTD/graphic/aftd_plot_mutations.png")
plot(aftd(mutations_matrix)$scores,main="Representation by aftd components",xlab="first comp",ylab="second comp",col="blue",pch=16)
dev.off()

# realisation de l'aftd avec la fonction "cmdscale()" de R
cmdscale(mutations_matrix)
png("C:/Users/Gttrégory/Dropbox/SY09/TP2/EX1 AFTD/graphic/cmdscale_plot_mutations.png")
plot(cmdscale(mutations_matrix),main="Representation by cmdscale components",xlab="first comp",ylab="second comp",col="red",pch=16)
dev.off()

# Question 2:       
for(i in 2:5)
{
  res <- cmdscale(mutations_matrix,k=i,eig=T)
  Qualite <- sum(res$eig[1:i])/sum(res$eig)
  print(Qualite)
  png("C:/Users/Grégory/Dropbox/SY09/TP2/EX1 AFTD/graphic/qualite_plot_mutations.png")
  plot(Shepard(mutations_matrix,res$points),pch=16)
  dev.off()
}

for(i in 2:5)
{
  res <- cmdscale(mutations_matrix,k=i,eig=T)
  Qualite <- sum(res$eig[1:i])/sum(res$eig)
  print(Qualite)
}



