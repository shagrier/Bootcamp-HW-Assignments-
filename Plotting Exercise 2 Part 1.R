setwd("C:/Users/Shalanda/Documents/R Bootcamp") 
snpsDataframe= read.table('hapmap_CEU_r23a_chr2_ld-2.txt', header=TRUE)
dim(snpsDataframe)

head(snpsDataframe)

names(snpsDataframe)

row.names(snpsDataframe)

snps=as.matrix(snpsDataframe)

testSNP=snps["rs218206_G",]

table(testSNP)

het=sum(testSNP==1)/length(testSNP)

testSNP=snps["rs6717613_A",]

table(testSNP)
testSNP==1
length(testSNP)
is.na(testSNP)

het=sum(testSNP==1)/length(testSNP)
het=sum(testSNP==1,na.rm=TRUE)/sum(!is.na(testSNP))

freq=sum(testSNP,na.rm=TRUE)/(2.0*sum(!is.na(testSNP)))

calc_freq=function(x){
  return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
  return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}

freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)


plot(freq,het,xlab="Frequency",ylab="Heterozygosity")
p=seq(0,0.5,by=0.05)   
points(p,2*p*(1-p),type="l",col=2) 

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
 
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  n=sum(obscnts)
  
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) 
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}

chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

cor.test(chisqs,chisqs2)
plot(chisqs,chisqs2)

pvals <- pchisq(chisqs,1,lower.tail=FALSE)
pvals

signifthres<-0.05
sum(pvals<signifthres) 
mean(pvals<signifthres)
#181 less than 0.05

signifthres<-0.01
sum(pvals<signifthres) 
mean(pvals<signifthres) 
#41 less than 0.01

signifthres<-0.001
sum(pvals<signifthres) 
mean(pvals<signifthres) 
#5 less than 0.001

num_pval <- length (pvals)
num_pval
#4014 pvals 

dpvals <- seq(1, 4014, by=1)
exp_pvals <- (dpvals/num_pval)
exp_pvals
log_exp_pvals <- -log10(exp_pvals)
log_exp_pvals


sort_pvals <- sort(pvals)
sort_pvals

log_sort_pvals <- -log10(sort_pvals)
log_sort_pvals


qqplot(log_exp_pvals, log_sort_pvals,xlab = "-log10(expected P- Value", ylab = "-log10(observed P-value)")
abline(0,1, col=2)

