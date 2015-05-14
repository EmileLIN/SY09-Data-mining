library(MASS)
library(Hmisc)

iris_quant <- iris[1:4]
n <- dim(iris_quant)[1]

#Question 01
par(mfrow=c(1,1))
C <- princomp(iris_quant)

res2 <- kmeans(iris_quant,2)
png("./Graphics/Q1_k2.png")
plot(C$scores[,1:2],col=res2$cluster,pch=16,main="kmeans of 2 classes")
points(res2$centers,col=1:2,pch=8)
dev.off()

#index <- which(res2$cluster==1)
#points_class1 <- iris_quant[index,]
#plot(points_class1)

res3 <- kmeans(iris_quant,3)
png("./Graphics/Q1_k3.png")
plot(C$scores,col=res3$cluster,pch=16,main="kmeans of 3 classes")
points(res3$centers,col=1:3,pch=8)
dev.off()

res4 <- kmeans(C$scores,4)
png("./Graphics/Q1_k4.png")
plot(C$scores,col=res4$cluster,pch=16,main="kmeans of 4 classes")
points(res4$centers,col=1:4,pch=8)
dev.off()

C_source <- princomp(iris_quant)
png("./Graphics/Q1_source_data.png")
plot(C_source$scores[,1:2],col=iris$Species,main="The classification of source data",pch=16)
dev.off()



#Question 02

png("./Graphics/Q2_k3.png")
par(mfrow=c(2,2))
for(i in 1:4)
{
	res <- kmeans(iris_quant,3)
	
	plot(C$scores,col=res$cluster,pch=16)
	points(res$centers,col=1:3,pch=8)
	title(main="Kmeans of 3 class")
	
	print(res$tot.withinss)

}
dev.off()

res_inertie <- rep(0,length=100)
for(i in 1:100)
{
  res <- kmeans(iris_quant,3)
  
  res_inertie[i] <- res$tot.withinss
  
}

hist(res_inertie)

#Question 03
#Firstly, we make a sample of 100 
sample_iris <- iris_quant[sample(1:n,100,replace=F),]
C_sample <- princomp(sample_iris)

#Make a loop to do k-means
par(mfrow=c(1,1))
index <- c(2:10)
inertie <- matrix(0,nrow=100,ncol=9)
for(i in 1:100)
{
  for(k in 2:10)
  {
    res <- kmeans(sample_iris,k)
    inertie[i,k-1] = res$tot.withinss 
  }
}
inertie_avg <- colMeans(inertie)

#Draw a plot to describe this
png("./Graphics/Q3_Inertie_total.png")
plot(index,inertie_avg,col = "blue",pch=17,main="Intra Inertie en fonction de K")
minor.tick(ny=10)
dev.off()



#Question 04



