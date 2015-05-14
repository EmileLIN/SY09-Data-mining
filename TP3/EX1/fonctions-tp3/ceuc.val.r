ceuc.val <- function(mu,Xtst)
{
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  nb_g <- dim(mu)[1]
  
  #Step 1 Calculate the distance between chaque points and every groups' gravity center
  #Step 2 Choose the minimal distance to determine which class the point belongs to
  dist <- matrix(0,nrow = n,ncol = nb_g)
  class <- rep(0, length=n)
  for(i in 1:n)
  {
    for(k in 1:nb_g)
    {
       dist[i,k] <- dist(rbind(Xtst[i,],mu[k,]))
    }
    class[i] <- which(dist[i,] == min(dist[i,]))
  }
  
  #print(class)
  return (class)
}
