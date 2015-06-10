#La fonction pour calculer les probabilites a priori

source("../fonctions-tp4/mvdnorm.r")

ad.val <- function(model,Xapp,zapp,Xtst)
{
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  
  #Faire l'apprentissage 
  res <- model(Xapp,zapp)
  #print("result is ")
  #print(res)
  
  #calculer la fonction densite 
  #classe1
  dens1_x <- mvdnorm(Xtst,res$mu1,res$sigma1)
  
  #classe2
  dens2_x <- mvdnorm(Xtst,res$mu2,res$sigma2)
  
  #densite general
  dens_x <- dens1_x * res$p1 + dens2_x * res$p2
  
  #propability 
  proba1 <- dens1_x * res$p1 / dens_x
  proba2 <- dens2_x * res$p2 / dens_x
  
  proba <- cbind(proba1,proba2)
  
  return (proba)

  
}