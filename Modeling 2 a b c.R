

rFun <-function(r,N,ttMax){
NN <- matrix (NA,ttMax +1) 
NN [1] <- N 

for (tt in 1:ttMax){
   NN[tt+1]= NN[tt]*exp(r*(1-NN[tt]/K))
}

plot(1:(ttMax+1), NN, lty=2, type='l',xlab="density",ylab="population size")
return(NN)
}

output<-rFun(r=-.5, N=30,ttMax=100)
#population decreases to n=0 (r=-.5, N=30, ttMax=100)
#population approaches stable equilibrium at n*=K, w/out oscillations (r=.5, N=15, ttMax=100)
#deaying around oscilations around n*=K (r=2, N=63, ttMax=100)
#persistent, regular oscillations (r=2.3, N=75, ttMax=100)
#crazy, random-looking fluctuations(chaos)(r=4, N=35, ttMax=100)

N <- 30
ttMax <-100

RR <- c(-7, 0.2, 2.5, 3.4, 4,6)
par(mfrow=c(2,3))

for (tt in 1:6){
 rFun(RR[tt], N, ttMax)
}





