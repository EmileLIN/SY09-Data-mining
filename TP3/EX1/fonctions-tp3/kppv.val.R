source("./fonctions-tp3/kppv.app.r")

kppv.val <- function(Xapp,zapp,K,Xtst)
{
  n <- dim(Xapp)[1]
  app_len <- dim(Xapp)[1]
  tst_len <- dim(Xtst)[1]
  
  class <- rep(0,tst_len)
  for(i in 1:tst_len)
  {
    dist <- rep(0,app_len)
    for(j in 1:app_len)
    {
      dist[j] <- dist(rbind(Xapp[j,],Xtst[i,])) 
    }
    
    Xapp_dist <- cbind(Xapp,dist)
    
    index <- indexOf_K_Minimum(Xapp_dist,K)
    class[i] <- class_decision(zapp,index,K) 
  }
  
  return (class)
  
}