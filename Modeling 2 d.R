N <- 20
K <-1000
rFun <-function(r,N,ttMax,K){
  NN <- matrix (NA,ttMax +1) 
  NN [1] <- N 
  
  for (tt in 1:ttMax){
    NN[tt+1]= NN[tt]*exp(r*(1-NN[tt]/K))
  }
  
  plot(1:(ttMax+1), NN, lty=2, type='l',xlab="density",ylab="population size")
  return(NN)
}
nVec <-rFun(r=.5,N= 20,ttMax = 300,K=1000)
which(nVec >500)[1]
min(which(nVec>=500))
