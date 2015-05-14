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

aftd <- function(Dis)
{
  aftd <- list()
  Dis <- as.matrix(Dis)
  
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

library(MASS)
#Question 01
#Read data
if (FALSE)
{
mutations <- read.table("mutations2.txt",header = F,row.name=1)
mutations <- as.dist(mutations)

res_aftd <- aftd(mutations)
res_cmdscale <- cmdscale(mutations)

par(mfrow=c(2,2))
plot(res_aftd$scores,main="The representation of res by function aftd",col = "red",pch=16)
plot(res_cmdscale,main="The representation of res by function cmdscale",col="blue",pch=16)
}

#Question 02
par(mfrow=c(1,1))

	res <- cmdscale(mutations,k=2,eig=T)
	Qualite <- sum(res$eig[1:i])/sum(res$eig)
	print(Qualite)
	
  png("./graphic/shepard2.png")
	plot(Shepard(mutations,res$points),pch=16,main="Shepard plot on espece AFTD k=2")
  abline(0,1)
  dev.off()

res <- cmdscale(mutations,k=14,eig=T)
Qualite <- sum(res$eig[1:i])/sum(res$eig)
print(Qualite)

png("./graphic/shepard14.png")
plot(Shepard(mutations,res$points),pch=16,main="Shepard plot on espece AFTD k=14")
abline(0,1)
dev.off()












