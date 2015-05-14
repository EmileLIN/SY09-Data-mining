#Load data 
student_marks <- matrix(c(6.0,6.0,5.0,5.5,8.0,8.0,8.0,8.0,8.0,9.0,6.0,7.0,11.0,9.5,11.0,14.5,14.5,15.5,15.0,8.0,14.0,
                          14.0,12.0,12.5,10.0,11.0,10.0,5.5,7.0,13.0,5.5,7.0,14.0,11.5,10.0,
                          13.0,12.5,8.5,9.5,12.0,9.0,9.5,12.5,12.0,18.0),nrow=9,byrow=T)

colnames(student_marks) <- c("math","scie","fran","lati","d-m")
rownames(student_marks) <- c("Jean","Aline","Annie","Monique","Didier","Andre","Pierre",
                             "Brigette","Evelyne")

#Scaling
marks_centre <- scale(student_marks,center = T,scale = F)

#Cov-variance
marks_cov <- 1/9 *t(marks_centre) %*% marks_centre

#Diago
mark_diago <- eigen(marks_cov)

#Principal Components
mark_c <- marks_centre %*% mark_diago$vectors
trans <- matrix(-1,nrow=9,ncol = 5)
mark_c <- mark_c * trans

#Plot for first plan
png("./Graphic/mark_first_plan.png")
plot(mark_c[,1],mark_c[,2],main="representatio of first two components",xlab="Comp1",ylab="Comp2")
text(mark_c[,1],mark_c[,2],pos=1,labels = c("Jean","Aline","Annie","Monique","Didier","Andre","Pierre",
                                      "Brigette","Evelyne"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

#Same thing if we use function princomp
res <- princomp(marks_centre)

#Presentation by plot, like we do before

#Presentaton by biplot
png("./Graphic/biplot_mark.png")
biplot(res,main="Representation by biplot for Mark data")
dev.off()

#Correlation
cor <- cor(marks_centre,res$scores)
png("./Graphic/marks_first_plan.png")
plot(cor[,1],cor[,2],main="correlation of first plan",xlab="Comp1",ylab="Comp2",xlim=c(-1,1),ylim=c(-1,1))
text(cor[,1],cor[,2],pos=1,labels = c("math","scie","fran","lati","d-m"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()

png("./Graphic/marks_second_plan.png")
plot(cor[,1],cor[,3],main="correlation of second plan",xlab="Comp1",ylab="Comp3",xlim=c(-1,1),ylim=c(-1,1))
text(cor[,1],cor[,3],pos=1,labels = c("math","scie","fran","lati","d-m"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()

png("./Graphic/marks_third_plan.png")
plot(cor[,2],cor[,3],main="correlation of third plan",xlab="Comp2",ylab="Comp3",xlim=c(-1,1),ylim=c(-1,1))
text(cor[,2],cor[,3],pos=1,labels = c("math","scie","fran","lati","d-m"))
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
symbols(0, 0, circles = 0.9, inches = F, add = T)
dev.off()




