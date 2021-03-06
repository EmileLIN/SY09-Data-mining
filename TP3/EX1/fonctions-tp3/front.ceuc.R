front.ceuc <- function(ceuc.val, mu, Xapp, zapp)
{
	minX <- min(Xapp[,1])-1
	maxX <- max(Xapp[,1])+1
	minY <- min(Xapp[,2])-1
	maxY <- max(Xapp[,2])+1
	# grille d'affichage 
	grilleX <- seq(from=minX,to=maxX,by=0.01)
	naffX <- length(grilleX)
	grilleY <- seq(from=minY,to=maxY,by=0.01)
	naffY <- length(grilleY)

	grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))

	# calcul des valeurs de la fonction 
	valf <- ceuc.val(mu, grille)
	plot(Xapp, col=c("red","green","blue","magenta","orange")[zapp],main="Euclidiene distance decision graph")
	contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
