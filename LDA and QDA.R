dat=read.csv("stt592dat.csv", header=T)
attach(dat)
names(dat)

dat=data.frame(chartn,ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]
dim(dat)
attach(dat)
names(dat)

install.packages('nortest')
library(MASS)
library(nortest)

#Linear Discriminant Analysis
l=lda(ptsd~aa*da,data=dat)
lpred=predict(l)
lclass=lpred$class
acctab=table(lclass,ptsd);acctab
accuracy=sum(diag(acctab))/sum(acctab);accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

length(ptsd)*0.8
dat[493,1]
train=(chartn<31464)
test=dat[!train,]
ptest=ptsd[!train]

ltrain=lda(ptsd~aa*da,data=dat,subset=train)
lpred=predict(ltrain,test)
lclass=lpred$class
acctab=table(lclass,ptest)
acctab
accuracy=sum(diag(acctab))/sum(acctab);accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

#Quadratic Discriminant Analysis
q=qda(ptsd~aa*da,data=dat)
qpred=predict(q)
qclass=qpred$class
acctab=table(qclass,ptsd);acctab
accuracy=sum(diag(acctab))/sum(acctab);accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

qtrain=qda(ptsd~aa*da,data=dat,subset=train)
qpred=predict(qtrain,test)
qclass=qpred$class
acctab=table(qclass,ptest)
acctab
accuracy=sum(diag(acctab))/sum(acctab); accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

#Validating Assumptions
##Normality of Variables
par(mfrow=c(3,2))
aaf=ifelse(ptsd==F,aa,NA);daf=ifelse(ptsd==F,da,NA);aadaf=ifelse(ptsd==F,aa*da,NA)
aat=ifelse(ptsd==T,aa,NA);dat=ifelse(ptsd==T,da,NA);aadat=ifelse(ptsd==T,aa*da,NA)

hist(aaf,col='lightgreen',main='No PTSD Diagnosis',xlab='TSI AA T-score');hist(aat,col='lightgreen',main='No PTSD Diagnosis',xlab='TSI AA T-score')
hist(daf,col='lightblue',main='No PTSD Diagnosis',xlab='TSI DA T-score');hist(dat,col='lightblue',main='No PTSD Diagnosis',xlab='TSI DA T-score')
hist(aadaf,col='lavender',main='No PTSD Diagnosis',xlab='TSI AA*DA T-score');hist(aadat,col='lavender',main='No PTSD Diagnosis',xlab='TSI AA*DA T-score')
qqnorm(aaf);qqline(aaf);qqnorm(aat);qqline(aat)
qqnorm(daf);qqline(daf);qqnorm(dat);qqline(dat)
qqnorm(aadaf);qqline(aadaf);qqnorm(aadat);qqline(aadat)
dev.off()

ad.test(aaf);ad.test(daf);ad.test(aadaf)
ad.test(aat);ad.test(dat);ad.test(aadat)

##Equal Variance of Variables b/t Groups
var.test(aaf,aat);var.test(daf,dat);var.test(aadaf,aadat)
