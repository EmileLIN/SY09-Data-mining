prob.ad <- function(model,name,X, z, niveaux)
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

    # calcul des valeurs de la fonction 
    valf <- ad.val(model,X,z,grille)
    valf <- valf[,1]
    
    pngname <- paste(name$path,name$filename,".png",sep = '')
    title <- paste("La frontiere de decision pour le models ",name$modelname,sep = '')
    
    png(pngname)
    plot(X, col=c("red","green","blue","magenta","orange")[z],pch=16,main=title)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
    dev.off()
}
