#Read data
mutations <- read.table("mutations2.txt",header = F,row.names=1)
mutations <- as.dist(mutations)

repre <- cmdscale(mutations,k=5,eig=T)

#Question01
inertie_total = rep(0,length=100)
png("Graphics/Q1.png")
par(mfrow=c(2,2))

for(i in 1:4)
{
  res <-kmeans(repre$points,3)
  
  inertie_total[i] <- res$tot.withinss;
  
  plot(repre$points[,1:2],col=res$cluster,main="La graphe de k-means",pch=16)
  points(res$centers,col=1:3,pch=8)
 

}
dev.off()

par(mfrow=c(1,1))
for(i in 1:100)
{
  res <-kmeans(repre$points,3)
  
  inertie_total[i] <- res$tot.withinss;
  
  
  #print("Les intra inertie est ")
  #print(res$withinss)
  
  #print("La intra inertie totale est ")
  #print(res$tot.withinss)
}
print(inertie_total)

png("./Graphics/Q2_histo.png")
hist(inertie_total)
dev.off()

