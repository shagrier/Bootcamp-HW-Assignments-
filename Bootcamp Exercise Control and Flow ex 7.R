hey <- matrix(NA, nrow=6, ncol=3)
Adjbank = bankAccounts + income - house - food -fun 
bankAccounts <- c(10, 9.2, 5.3);
interestRate <- 0.0525
house <- c(4.8, 3.8, 5.7);
food <- c(3.5, 4.3, 5.0);
fun <- c(7.8, 2.1, 10.5);
income <- c(21, 21, 21);
hey[1,] <- Adjbank
for (j in 2015:2020){
  for (i in 1:length(bankAccounts)){
    hey[j+1, i]<- interestRate*hey[j,i]+ hey[j,i]
    if (j %% odd){
      fund <- (bankAccounts [1]+ 5000)
      fund2 <- (bankAccounts [3] + 5000)
    }
    }
}

fund
fund2
