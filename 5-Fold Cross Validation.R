#Allen Phelps
dat=read.csv("stt592dat.csv", header=T)
attach(dat)

dat=data.frame(ptsd,aa,da,aa*da)
dat=dat[complete.cases(dat),]

library(boot)
library(MASS)

#Logistic Regression LOO CV
y=glm(ptsd~aa*da,data=dat,family=binomial)
error=cv.glm(dat,y)
lr1out=1-error$delta;lr1out

#Logistic Regression K-fold CV
set.seed(1)
accuracy=rep(0,5)
for (i in 1:5)
{
  y=glm(ptsd~poly(aa*da,i),data=dat,family=binomial)
  accuracy[i]=1-cv.glm(dat,y,K=5)$delta[1]
}
lr5out=accuracy;lr5out

#LDA K-fold CV
nfolds=5
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

lda5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  l=lda(ptsd~aa*da,data=dat)
  lpred=predict(l,test[,2:4])
  lclass=lpred$class
  table(lclass,test[,1])
  accuracy=mean(lclass==test[,1])
  lda5out=c(lda5out,accuracy)
}
print(lda5out)
mean(lda5out)
boxplot(lda5out,col='green')

#LDA LOO CV
n=dim(dat)[1]
foldi=sample(rep(1:n,length.out=n))
table(foldi)

lda1out=NULL
for(k in 1:n) 
{ 
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  l=lda(ptsd~aa*da,data=dat)
  lpred=predict(l,test[,2:4])
  lclass=lpred$class
  table(lclass,test[,1])
  accuracy=mean(lclass==test[,1])
  lda1out=c(lda1out,accuracy)
}
print(lda1out)
mean(lda1out)
boxplot(lda1out,col='green')

#QDA K-fold CV
nfolds=5
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

qda5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  q=qda(ptsd~aa*da,data=dat)
  qpred=predict(q,test[,2:4])
  qclass=qpred$class
  table(qclass,test[,1])
  accuracy=mean(qclass==test[,1])
  qda5out=c(qda5out,accuracy)
}
print(qda5out)
mean(qda5out)
boxplot(qda5out,col='green')

#QDA LOO CV
n=dim(dat)[1]
foldi=sample(rep(1:n,length.out=n))
table(foldi)

qda1out=NULL
for(k in 1:n)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  q=qda(ptsd~aa*da,data=dat)
  qpred=predict(q,test[,2:4])
  qclass=qpred$class
  table(qclass,test[,1])
  accuracy=mean(qclass==test[,1])
  qda1out=c(qda1out,accuracy)
}
print(qda1out)
mean(qda1out)
boxplot(qda1out,col='green')

#Dave Hiltbrand
dat=read.csv("stt592dat.csv", header=T)
attach(dat)
names(dat)

dat=data.frame(chartn,ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]
dim(dat)

library(class)

## k-Fold CV to determine best K for KNN
n=dim(dat)[1]; m=dim(dat)[2]; print(c(m,n))

n_fold<-5;
rep(1:n_fold, length.out = n) 
folds_i <- sample(rep(1:n_fold, length.out = n))
table(folds_i)

Final.OUT=NULL
for (i in 1:30)
{
  OUT.KNN=NULL
  for (j in 1:n_fold) 
  {
    test.ID <- which(folds_i == j)
    train_X <- dat[-test.ID, c("aa","da")]
    train_Y <- dat[-test.ID, 2]
    test_X <- dat[test.ID, c("aa","da")]
    test_Y <- dat[test.ID, 2]
    knn.pred=knn(train_X, test_X, train_Y, k=i) 
    Accuracy=mean(knn.pred==test_Y)
    OUT.KNN=c(OUT.KNN, Accuracy)
  }
  Final.OUT=rbind(Final.OUT, OUT.KNN)  
}  
print(Final.OUT)
apply(Final.OUT, 1, mean)
rowMeans(Final.OUT)
max(rowMeans(Final.OUT))
boxplot(t(Final.OUT), col=rainbow(10))

## KNN
set.seed(1)
x=cbind(dat$aa,dat$da)
k=knn(x,x,dat$ptsd,k=20,prob=T)
table(k,dat$ptsd)
mean(k==dat$ptsd)

##k-fold:5 CV for KNN
n_fold<-5; 
rep(1:n_fold, length.out = n) 
folds_i <- sample(rep(1:n_fold, length.out = n))
table(folds_i)

OUT.KNN=NULL
for (j in 1:n_fold) 
{
  test.ID <- which(folds_i == j)
  train_X <- dat[-test.ID, c("aa","da")]
  train_Y <- dat[-test.ID, 2]
  test_X <- dat[test.ID, c("aa","da")]
  test_Y <- dat[test.ID, 2]
  knn.pred=knn(train_X, test_X, train_Y, k=20) 
  table(knn.pred,test_Y)
  Accuracy=mean(knn.pred==test_Y)
  OUT.KNN=c(OUT.KNN, Accuracy)
}
print(OUT.KNN)
mean(OUT.KNN) ##overall accuracy
boxplot(OUT.KNN,col="orange")

## LOO CV for KNN
folds_i <- sample(rep(1:n, length.out = n))
table(folds_i)

OUT.KNN1=NULL
for (j in 1:n) 
{
  test.ID <- which(folds_i == j)
  train_X <- dat[-test.ID, c("aa","da")]
  train_Y <- dat[-test.ID, 2]
  test_X <- dat[test.ID, c("aa","da")]
  test_Y <- dat[test.ID, 2]
  knn.pred=knn(train_X, test_X, train_Y, k=20) 
  table(knn.pred,test_Y)
  Accuracy=mean(knn.pred==test_Y)
  OUT.KNN1=c(OUT.KNN1, Accuracy)
}
print(OUT.KNN1)
mean(OUT.KNN1) ##overall accuracy
boxplot(OUT.KNN1,col="orange")

boxplot(lr5out,lda5out,qda5out,OUT.KNN,col=c('yellow','lightgreen','lightblue','lavender'),main='5-fold Cross Validation Comparison',names=c('LR','LDA','QDA','KNN'),xlab='Classification Method',ylab='Overall Accuracy')
out1=c(mean(lr1out),mean(lda1out),mean(qda1out),mean(OUT.KNN1))
out1
