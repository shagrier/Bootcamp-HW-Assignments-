##modeling exercise 2e 
rFun <-function(r,N,ttMax,k){
  NN <- matrix (NA,ttMax +1) 
  NN [1] <- N 
  
  for (tt in 1:ttMax){
    NN[tt+1]= NN[tt]*exp(r*(1-NN[tt]/K))
  }
  
  plot(1:(ttMax+1), NN, lty=2, type='l',xlab="density",ylab="population size")
  return(NN)
}

N <-5
ttMax <- 100
K <- 500


r <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
for (tt in 1:9){
  rFun(r[tt], N, ttMax,K)
}



  