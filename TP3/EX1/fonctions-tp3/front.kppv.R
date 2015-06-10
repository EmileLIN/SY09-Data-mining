front.kppv <- function(kppv.val, K, Xapp, zapp)
{
	minX <- min(Xapp[,1])-1
	maxX <- max(Xapp[,1])+1
	minY <- min(Xapp[,2])-1
	maxY <- max(Xapp[,2])+1
	# grille d'affichage 
	grilleX <- seq(from=minX,to=maxX,by=0.1)
	naffX <- length(grilleX)
	grilleY <- seq(from=minY,to=maxY,by=0.1)
	naffY <- length(grilleY)

	grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  print(grille)

	# calcul des valeurs de la fonction 
	valf <- kppv.val(Xapp, zapp, K, grille)
	plot(Xapp, col=c("red","green","blue","magenta","orange")[zapp],main="Le most closed neibour decision graph")
	contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
