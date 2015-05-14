

#Data
data <- matrix(c(3,4,3,1,4,3,2,3,6,2,1,2),nrow=4,byrow=TRUE)
nb_ind <- dim(data)[1]
nb_var <- dim(data)[2]

Dp <- diag(nb_ind)/nb_ind
M  <- diag(nb_var)

#Scale the data
#data_centre <- scale(data,center=F,scale=F)
data_centre <- data

#Calcul the corvariance matrix
data_cov <- 1/4 * t(data_centre) %*% data_centre   
#data_cov1 <- cov(data_centre) # That is not very correct 

#Diagonalize the covariance matrix to get Eigenvalues and Eigenvectors
diag = eigen(data_cov)

#Question01

#Axes 
diag$vectors

#Percentage of Inerties
I_total <- sum(diag$values)
I_axe1 <-  diag$values[1] / I_total *100
I_axe2 <-  diag$values[2] / I_total *100
I_axe3 <-  diag$values[3] / I_total *100

#Question02
#Cacul the principal components
Comp <- data_centre %*% M %*% diag$vectors

#Compare the Function princomp
Comp_true <- princomp(data)

#Representation in First plan
png("./Graphic/ACP_rep.png")
plot(Comp[,1],Comp[,2],main="Representation by the first plan",xlab="axe1",ylab="axe2")
abline(lty=1,a=0,b=0,col=1)
abline(lty=1,a=0,b=1000000,col=1)
dev.off()

#Question03

#Question04
#k=1
X1 <- Comp[,1] %*% t(diag$vectors[,1])
X2 <- X1 + Comp[,2] %*% t(diag$vectors[,2])
X3 <- X2 + Comp[,3] %*% t(diag$vectors[,3])

















