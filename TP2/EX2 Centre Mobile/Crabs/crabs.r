#Read data

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
crabsquant <- crabsquant/matrix(rep(crabsquant[,4],dim(crabsquant)[2]),
                                nrow=dim(crabsquant)[1],byrow=F)
crabsquant <- crabsquant[,-4]

#Question 01
C_crab <- princomp(crabsquant)
res <- kmeans(crabsquant,4)
png("./Graphics/Q1.png")
plot(C_crab$scores,col=res$cluster,main="La partition des crabes en K-means",pch=16)
dev.off()

#Question 02 
png("./Graphics/Q2_espece.png")
plot(C_crab$scores,col=crabs$sp,main="La partition des crabes par spieces en reel",pch=16)
dev.off()

png("./Graphics/Q2_sexe.png")
plot(C_crab$scores,col=crabs$sex,main="La partition des crabes par sexe en reel",pch=16)
dev.off()
