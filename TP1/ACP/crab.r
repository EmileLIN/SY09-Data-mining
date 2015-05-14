#Data crabs
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]

#Question01
Comp_crab <- princomp(crabsquant)

#Barplot 
png("./Graphic/Crab_Q1_barplot.png")
barplot(Comp_crab$sdev,main="Barplot for engin values of crab data")
dev.off()

png("./Graphic/Crab_Q1_acp.png")
biplot(Comp_crab,main="Represenation of Crabs by two first components",xlab="Comp1",ylab="Comp2")
dev.off()

cor <- cor(crabsquant,Comp_crab$scores)
png("./Graphic/Crab_Q2_acp_Correlation_without_treatment.png")
plot(cor[,1],cor[,2],main="correlation of first two components without treatment",xlab="Comp1",ylab="Comp2",xlim=c(-1,1),ylim=c(-1,1))
text(cor[,1],cor[,2],pos=1,labels = c("FL","RW","CL","CW","BD"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()


#Question02
#pourcentage
crabs_pourcent <- crabsquant/rowSums(crabsquant)
Comp_pourcent <- princomp(crabs_pourcent)

png("./Graphic/crab_Q2_barplot_pourcentage.png")
barplot(Comp_pourcent$sdev,main="Barplot for engin values of crab data of pourcentage")
dev.off()

png("./Graphic/Crab_Q2_acpbiplot_pourcentage.png")
biplot(Comp_pourcent,main="Biplot of Crabs of pourcentage")
dev.off()

cor_pourcent <- cor(crabsquant,Comp_pourcent$scores)
png("./Graphic/Crab_Q2_acp_Correlation_pourcentage.png")
plot(cor_pourcent[,1],cor_pourcent[,2],main="correlation of first two components with pourcentage",xlab="Comp1",ylab="Comp2",xlim=c(-1,1),ylim=c(-1,1))
text(cor_pourcent[,1],cor_pourcent[,2],pos=1,labels = c("FL","RW","CL","CW","BD"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()



#Remove CL
crabs_notCL <- crabsquant/crabsquant$CL
crabs_notCL <- crabs_notCL[,-3]
Comp_notCL <- princomp(crabs_notCL)

png("./Graphic/crab_Q2_barplot_notCL.png")
barplot(Comp_notCL$sdev,main="Barplot for engin values of crab data removing CL")
dev.off()

png("./Graphic/Crab_Q2_acpbiplot_notCL.png")
biplot(Comp_notCL,main="Biplot of Crabs removing CL")
dev.off()

cor_notCL <- cor(crabsquant[,-3],Comp_notCL$scores)
png("./Graphic/Crab_Q2_acp_Correlation_removingCL.png")
plot(cor_notCL[,1],cor_notCL[,2],main="correlation of first two components without CL",xlab="Comp1",ylab="Comp2",xlim=c(-1,1),ylim=c(-1,1))
text(cor_notCL[,1],cor_notCL[,2],pos=1,labels = c("FL","RW","CW","BD"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()





