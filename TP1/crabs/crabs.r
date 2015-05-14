#Read Crabs Data

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

#Question01
#A Descriptive Boxplot for all data
png("./graphics/Descriptive.png")
boxplot(crabsquant,main="A Description on Crab data",ylab="crab data")
dev.off()

attach(crabs)
#FL with spieces
png("./graphics/FL_sp.png")
boxplot(FL[sp=="B"],FL[sp=="O"],col=c("red","blue"),main="Etude de FL en fonction d'especes")
dev.off()

png("./graphics/RW_sp.png")
boxplot(RW[sp=="B"],RW[sp=="O"],col=c("red","blue"),main="Etude de RW en fonction d'especes")
dev.off()

png("./graphics/CL_sp.png")
boxplot(CL[sp=="B"],CL[sp=="O"],col=c("red","blue"),main="Etude de CL en fonction d'especes")
dev.off()

png("./graphics/CW_sp.png")
boxplot(CW[sp=="B"],CW[sp=="O"],col=c("red","blue"),main="Etude de CW en fonction d'especes")
dev.off()

png("./graphics/BD_sp.png")
boxplot(BD[sp=="B"],BD[sp=="O"],col=c("red","blue"),main="Etude de BD en fonction d'especes")
dev.off()

#Sex
png("./graphics/FL_sex.png")
boxplot(FL[sex=="M"],FL[sex=="F"],col=c("red","blue"),main="Etude de FL en fonction de sex")
dev.off()

png("./graphics/RW_sex.png")
boxplot(RW[sex=="M"],RW[sex=="F"],col=c("red","blue"),main="Etude de RW en fonction de sex")
dev.off()

png("./graphics/CL_sex.png")
boxplot(CL[sex=="M"],CL[sex=="F"],col=c("red","blue"),main="Etude de CL en fonction de sex")
dev.off()

png("./graphics/CW_sex.png")
boxplot(CW[sex=="M"],CW[sex=="F"],col=c("red","blue"),main="Etude de CW en fonction de sex")
dev.off()

png("./graphics/BD_sex.png")
boxplot(BD[sex=="M"],BD[sex=="F"],col=c("red","blue"),main="Etude de BD en fonction de sex")
dev.off()


#Queston 02
#Crab Spiece
png("./graphics/cor_sp.png")
plot(crabsquant,col=crabs$sp,main="representation for spieces")
dev.off()

#Crab Sex
png("./graphics/cor_sex.png")
plot(crabsquant,col=crabs$sex,main="representation for sex")
dev.off()

tab_cor <- cor(crabsquant)

#We obeserve the correlation table and we found that all the variables are very relative.
#It is the "Effet Taille" 
#We should use some method to eliminate this effect，as "Centre en ligne"

sum <- rowSums(crabsquant);
crabsquant_new <- crabsquant/sum

tab_cor_new <- cor(crabsquant)

#Crab Spiece
png("./graphics/cor_sp_new.png")
plot(crabsquant_new,col=crabs$sp,main="representation for spieces of new data")
dev.off()

#Crab Sex
png("./graphics/cor_sex_new.png")
plot(crabsquant_new,col=crabs$sex,main="representation for sex of new data")
dev.off()




