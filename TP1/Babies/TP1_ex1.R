#Babies
#Read Babies' data

babies<-read.table("babies23.txt",header=T)
babies<-babies[c(7,5,8,10,12,13,21,11)]
names(babies)<-c("bwt","gestation","partity","age","height","weight","smoke","education")

#Replace some unaccessible data by NA

babies[babies$bwt == 999, 1] <- NA
babies[babies$gestation == 999, 2] <- NA
babies[babies$age == 99, 4] <- NA
babies[babies$height == 99, 5] <- NA
babies[babies$weight == 999, 6] <- NA
babies[babies$smoke == 9, 7] <- NA
babies[babies$education == 9, 8] <- NA 

#Declare some factors
babies$smoke <- factor(c("NonSmoking","Smoking","NonSmoking","NonSmoking")[babies$smoke+1])
babies$education<-factor(babies$education,ordered=T)

#Question01
#numeric resume between bwt and smoke
tab_bwt_ns <- summary(babies$bwt[babies$smoke == 'NonSmoking'])
tab_bwt_s <- summary(babies$bwt[babies$smoke == 'Smoking'])

tab_gest_ns <- summary(babies$gestation[babies$smoke == 'NonSmoking'])
tab_gest_s<-summary(babies$gestation[babies$smoke == 'Smoking'])

summary(babies$ed[babies$smoke == 'NonSmoking'])
summary(babies$ed[babies$smoke == 'Smoking'])

#Graphic

attach(babies)

#bwt and smoke
boxplot(bwt~smoke,col=c("red","blue"))
#gestation and smoke
boxplot(gestation~smoke)

#histogram




 