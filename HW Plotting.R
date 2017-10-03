get_heights <-function(n) {
  heights <- rnorm(n, mean = 69, sd = 10)
  return(mean(heights)) 
}
mean_heights_100 <- matrix(NA, nrow =1, ncol=1000)
for (i in 1:1000){
  mean_heights_100[i] <- get_heights(100)  
}

mean_heights_1000 <-matrix(NA, nrow=1, ncol=1000) 
for (i in 1:1000){
  mean_heights_1000[i] <-get_heights(1000)
} 

bins <-seq(64,73,by=0.5)
hist(mean_heights_100,breaks=bins)$breaks
hist(mean_heights_1000,breaks=bins)$breaks 

barplot(rbind(mean_heights_100, mean_heights_1000), col=c(2,4),beside=T,names.arg=seq(64,73,by=0.5), xlab="Mean Heights", ylab="Count")
legend(6,350,c(expression(paste(n, "=100")), expression(paste(n, "=1000"))),col=c(2,4),lwd=4)