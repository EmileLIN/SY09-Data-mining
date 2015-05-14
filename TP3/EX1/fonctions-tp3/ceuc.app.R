#classifer of eucliden distance

ceuc.app <- function(Xapp,zapp)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  g <- unique(zapp)
  nb_g <- length(unique(zapp)) # number of groups
   
  #Firstly, caluclate the gravity center for each class
  dist_center <- matrix(0,nrow = nb_g,ncol = p) 
  dist <- matrix(0,nrow = 0, ncol=p)
  for(k in 1:nb_g)
  {
    for(i in 1:n)
    {
      if(zapp[i] == g[k])
      {
        dist <- rbind(dist,Xapp[i,])
        #print(dist)
      }
    }
    dist_center[k,] <- colMeans(dist)
  }
  return (dist_center)
}






