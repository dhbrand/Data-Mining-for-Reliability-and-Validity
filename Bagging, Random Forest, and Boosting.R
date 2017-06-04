install.packages('gbm')
install.packages('randomForest')
install.packages('tree')
library(boot)
library(class)
library(gbm)
library(MASS)
library(randomForest)
library(tree)

dat=read.csv("stt592dat.csv", header=T)
names(dat)

ptsd=ifelse(dat[,13]==TRUE,'Yes','No')
dat=data.frame(ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]

#Classification Decision Tree
dim(dat)/2
set.seed(1)
traindat=sample(1:nrow(dat),308)
testdat=dat[-traindat,]
ytest=dat[,1][-traindat]

ytree=tree(ptsd~.,dat,subset=traindat)
summary(ytree)
plot(ytree)
text(ytree,pretty=0)

ypred=predict(ytree,testdat,type='class')
table(ypred,ytest)
mean(ypred==ytest)
sd(ypred==ytest)

set.seed(1)
cvytree=cv.tree(ytree,FUN=prune.misclass)
cvytree

yprune=prune.misclass(ytree,best=4)
plot(yprune)
text(yprune,pretty=0)

ypred=predict(yprune,testdat,type='class')
table(ypred,ytest)
mean(ypred==ytest)
sd(ypred==ytest)

#Bagging LOOCV
n=dim(dat)[1]
set.seed(1)
foldi=sample(rep(1:n,length.out=n))
table(foldi)

bag1out=NULL
for(k in 1:n)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  ybag=randomForest(ptsd~.,data=train,mtry=13,ntree=500,importance=T)
  bagpred=predict(ybag,test)
  accuracy=mean(bagpred==test[,1])
  bag1out=c(bag1out,accuracy)
}
print(bag1out)
mean(bag1out) #0.6829268
sd(bag1out) #0.465715
boxplot(bag1out,col='green')

#Bagging 5-fold CV
nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

bag5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  ybag=randomForest(ptsd~.,data=train,mtry=13,ntree=500,importance=T)
  bagpred=predict(ybag,test)
  accuracy=mean(bagpred==test[,1])
  bag5out=c(bag5out,accuracy)
}
print(bag5out)
mean(bag5out) #0.6780488
sd(bag5out) #0.03477927
boxplot(bag5out,col='green')

#Random Forest LOOCV
n=dim(dat)[1]
set.seed(1)
foldi=sample(rep(1:n,length.out=n))
table(foldi)

rf1out=NULL
for(k in 1:n)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  yrf=randomForest(ptsd~.,data=train,mtry=4,ntree=500,importance=T)
  rfpred=predict(yrf,test)
  accuracy=mean(rfpred==test[,1])
  rf1out=c(rf1out,accuracy)
}
print(rf1out)
mean(rf1out) #0.697561
sd(rf1out) #0.4596882
boxplot(rf1out,col='green')

#Random Forest 5-fold CV
nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

rf5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  yrf=randomForest(ptsd~.,data=train,mtry=4,ntree=500,importance=T)
  rfpred=predict(yrf,test)
  accuracy=mean(rfpred==test[,1])
  rf5out=c(rf5out,accuracy)
}
print(rf5out)
mean(rf5out) #0.7056911
sd(rf5out) #0.03009229
boxplot(rf5out,col='green')

#Boosting LOOCV
n=dim(dat)[1]
set.seed(1)
foldi=sample(rep(1:n,length.out=n))
table(foldi)

dat=read.csv("stt592dat.csv", header=T)
names(dat)

ptsd=ifelse(dat[,13]==TRUE,1,0)
dat=data.frame(ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]

boost1out=NULL
for(k in 1:n)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  yboost=gbm(ptsd~.,data=train,distribution='bernoulli',interaction.depth=2,n.trees=1000,shrinkage=0.01)
  boostpred=predict(yboost,test,n.trees=1000,type='response')
  boostpred=factor(ifelse(boostpred<=0.5,0,1))
  accuracy=mean(boostpred==test[,1])
  boost1out=c(boost1out,accuracy)
}
print(boost1out)
mean(boost1out) #0.699187
sd(boost1out) #0.4589848
boxplot(boost1out,col='green')

#Boosting 5-fold CV
nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

dat=read.csv("stt592dat.csv", header=T)
names(dat)

ptsd=ifelse(dat[,13]==TRUE,1,0)
dat=data.frame(ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]

boost5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[testi,]
  yboost=gbm(ptsd~.,data=train,distribution='bernoulli',interaction.depth=2,n.trees=1000,shrinkage=0.01)
  boostpred=predict(yboost,test,n.trees=1000,type='response')
  boostpred=factor(ifelse(boostpred<=0.5,0,1))
  accuracy=mean(boostpred==test[,1])
  boost5out=c(boost5out,accuracy)
}
print(boost5out)
mean(boost5out) #0.7121951
sd(boost5out) #0.03180099
boxplot(boost5out,col='green')

dat=read.csv("stt592dat.csv", header=T)
names(dat)

dat=data.frame(dat[,13],dat[,22],dat[,26],dat[,22]*dat[,26])
dat=dat[complete.cases(dat),]
names(dat)=c('ptsd','aa','da','aada')

#Logistic Regression LOOCV
n=dim(dat)[1]
set.seed(1)
foldi=sample(rep(1:n,length.out=n))
table(foldi)

lr1out=NULL
for(k in 1:n)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[!train,]
  ytrain=glm(ptsd~aa*da,data=dat,family=binomial)
  ptrain=predict(ytrain,test[,2:4],type='response')
  ptrain=ifelse(ptrain<=0.5,FALSE,TRUE)
  accuracy=(ptrain==test[,1])
  lr1out=c(lr1out,accuracy)
}
print(lr1out)
mean(lr1out) #0.6294581
sd(lr1out) #0.4829513
boxplot(lr1out,col='green')

#Logistic Regression 5-fold CV
nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=dim(dat)[1]))
table(foldi)

lr5out=NULL
for(k in 1:nfolds)
{
  testi=which(foldi==k)
  train=dat[-testi,]
  test=dat[!train,]
  ytrain=glm(ptsd~aa*da,data=dat,family=binomial)
  ptrain=predict(ytrain,test[,2:4],type='response')
  ptrain=ifelse(ptrain<=0.5,FALSE,TRUE)
  accuracy=mean(ptrain==test[,1])
  lr5out=c(lr5out,accuracy)
}
print(lr5out)
mean(lr5out) #0.7190422
sd(lr5out) #0.01809779
boxplot(lr5out,col='green')

#LDA LOOCV
n=dim(dat)[1]
set.seed(1)
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
mean(lda1out) #0.7089431
sd(lda1out) #0.4546195
boxplot(lda1out,col='green')

#LDA 5-fold CV
nfolds=5
set.seed(1)
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
mean(lda5out) #0.7089431
sd(lda5out) #0.0194103
boxplot(lda5out,col='green')

#QDA LOOCV
n=dim(dat)[1]
set.seed(1)
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
mean(qda1out) #0.6813008
sd(qda1out) #0.4663514
boxplot(qda1out,col='green')

#QDA 5-fold CV
nfolds=5
set.seed(1)
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
mean(qda5out) #0.6813008
sd(qda5out) #0.01563852
boxplot(qda5out,col='green')

#KNN LOOCV
set.seed(1)
folds_i <- sample(rep(1:n, length.out = n))
table(folds_i)

OUT.KNN1=NULL
for (j in 1:n)
{
  test.ID <- which(folds_i == j)
  train_X <- dat[-test.ID, c("aa","da")]
  train_Y <- dat[-test.ID, 1]
  test_X <- dat[test.ID, c("aa","da")]
  test_Y <- dat[test.ID, 1]
  knn.pred=knn(train_X, test_X, train_Y, k=20)
  table(knn.pred,test_Y)
  Accuracy=mean(knn.pred==test_Y)
  OUT.KNN1=c(OUT.KNN1, Accuracy)
}

print(OUT.KNN1)
mean(OUT.KNN1) #0.7056911
sd(OUT.KNN1) #0.4561025
boxplot(OUT.KNN1,col="orange")

#KNN 5-fold CV
n_fold<-5;
rep(1:n_fold, length.out = n)
set.seed(1)
folds_i <- sample(rep(1:n_fold, length.out = n))
table(folds_i)

OUT.KNN=NULL
for (j in 1:n_fold)
{
  test.ID <- which(folds_i == j)
  train_X <- dat[-test.ID, c("aa","da")]
  train_Y <- dat[-test.ID, 1]
  test_X <- dat[test.ID, c("aa","da")]
  test_Y <- dat[test.ID, 1]
  knn.pred=knn(train_X, test_X, train_Y, k=20)
  table(knn.pred,test_Y)
  Accuracy=mean(knn.pred==test_Y)
  OUT.KNN=c(OUT.KNN, Accuracy)
}

print(OUT.KNN)
mean(OUT.KNN) #0.702439
sd(OUT.KNN) #0.03752189
boxplot(OUT.KNN,col="orange")

boxplot(lr5out,lda5out,qda5out,OUT.KNN,bag5out,rf5out,boost5out,col=c(rainbow(7)),
        main='5-fold Cross Validation Comparison',names=c('LR','LDA','QDA','KNN','BAG','RF','BST'),
        xlab='Classification Method',ylab='Overall Accuracy')
boxplot(lr1out,lda1out,qda1out,OUT.KNN1,bag1out,rf1out,boost1out,col=c(rainbow(7)),
        main='Leave One Out Cross Validation Comparison',names=c('LR','LDA','QDA','KNN','BAG','RF','BST'),
        xlab='Classification Method',ylab='Overall Accuracy')
