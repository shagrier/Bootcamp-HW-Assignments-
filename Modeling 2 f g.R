#Modeling exercise f and g

# Joint sensitivity analysis for two parameters
# create vector of values for parameter r and K
# initialize vector to hold output
# output t equals 8
# write nested loop for parameter r followed by K.
#plot the results 


#use function from discrete model 

geoFun <- function(rd, N ,k){
  # initialize vector to hold output
  NN <- rep(NA,ttMax+1)
  NN [1] <- N #set first value to initial condition
  
  #use a loop to iterate the model the desired number of times 
  for (tt in 1:ttMax) {
    NN[tt+1] <- NN[tt]*(1+rd*(1-(NN[tt]/k)))
  }
}


N0<- 30
kVec <- seq(200,350, by= 50)
rdVec <- seq(-.5, 1.0, by= .5)
mrdVec <- rep(NA, 1, length(rdVec))
mkVec <- rep(NA, 1, length(kVec))
Tcollect <- 8

for (ii in 1:length(rdVec))
{ outputfun <-geoFun(rd=rdVec[ii],N = 30, K=kVec[jj])
  mrdVec[ii] <- outputfun[Tcollect]
for (jj in 1:length(mkVec))
{outputfun <-geoFun(rd=rdVec[ii], N = 30, K=kVec[jj] )
  mkVec[jj] <- outputfun[Tcollect]
}
  
plot()





rd_Vec <-1:10
k_Vec <- 1:10
ttMax <- ttmaxcollect <-8
x <- matrix(c(10,10))
mymat = matrix(rep(NA, 100),10,10)


for (ii in 1:length(rd_Vec)){
  
for (jj in 1:length(mk_Vec))
  Output <- geoFun(r = rd_Vec[ii], N = 30, k_Vec[jj])
  mymat[ii, jj] <- Output[ttmaxcollect]
}


