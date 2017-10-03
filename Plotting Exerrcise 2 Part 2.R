setwd("C:/Users/Shalanda/Documents/R Bootcamp")
zz = read.table('pheno.sim.2014-2.txt', header = TRUE)
zz
pheno=zz$glucose_mmolperL
pheno
quantile(pheno)
min(pheno)
max(pheno)
plot(density(pheno), col=2,lwd=4, xlab = "Phenotypes",xlim=c(2,11), main="Glucose Levels")
abline(v=quantile(pheno,.25), lty=2, lwd=3, col=1)
abline(v=quantile(pheno,.75), lty=2, lwd=3,col=4)
