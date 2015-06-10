source("./log.val.quadra.r")
source("./fonction.r")
prob.log2 <- function(param, X, z, niveaux, path, k)
{
   
    discretisation=50
    deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
    deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
    minX <- min(X[,1])-deltaX
    maxX <- max(X[,1])+deltaX
    minY <- min(X[,2])-deltaY
    maxY <- max(X[,2])+deltaY
    
    # grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
	  grille <- cbind(grille, grille[,1]*grille[,2], grille[,1]^2, grille[,2]^2)

    grille <- as.matrix(grille)
 
    # calcul des valeurs de la fonction 

    valf <- log.val.quadra(param, grille,2)$prob[,1]
    
    
    imagename <- paste(path,"Synth1_quadra_",k,".png",sep="")
    title <- paste("Synth1 Qudra Logistic Regression Graph ",k,sep="")
    png(imagename)
    plot(X, col=c("red","green","blue","magenta","orange")[z],pch=16,main=title)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
    dev.off()
}
