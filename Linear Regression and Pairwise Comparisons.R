dat=read.csv("stt592dat.csv", header=T)
attach(dat)
install.packages('nortest')
library(nortest)
library(MASS)

#Martha Simple Linear Regression (Clinical b/t Tests)

plot(bai,aa,col='blue',main='Beck Anxiety Inventory(BAI) vs Trauma Symptom Inventory(TSI)',pch=15,xlab='BAI',ylab='TSI Anxiety Arousal Scale')
y=lm(aa~bai)
abline(y,col='black',lwd=3)
summary(y)
par(mfrow=c(2,2))
plot(y);ad.test(y$residuals)
dev.off()

bai2=ifelse(bai>18&bai<64,bai,NA)
plot(bai2,aa,col='blue',main='Beck Anxiety Inventory (BAI) vs Trauma Symptom Inventory (TSI)',pch=15,xlab='BAI Anxiety Level',ylab='TSI Anxious Arousal T-score')
y2=lm(aa~bai2)
abline(y2,col='black',lwd=3)
summary(y2)
par(mfrow=c(2,2))
plot(y2);ad.test(y2$residuals)
dev.off()

bai3=ifelse(bai>63,bai,NA)
(length(bai)-sum(is.na(bai3)))/(sum(!is.na(bai)))

plot(bai2,pt,col='blue',main='Beck Anxiety Inventory (BAI) vs Minnesota Multiphasic Personality Inventory (MMPI)',pch=15,xlab='BAI Anxiety Level',ylab='MMPI Psychathenia T-score')
y3=lm(pt~bai2)
abline(y3,col='black',lwd=3)
summary(y3)
par(mfrow=c(2,2))
plot(y3);ad.test(y3$residuals)
dev.off()

plot(zds,d,col='green',pch=15,xlab='ZDS Level of Depression',ylab='TSI Depression T-score',main='Zung Depression Scale (ZDS) vs Trauma Symptom Inventory (TSI)')
y4=lm(d~zds)
abline(y4,col='black',lwd=3)
summary(y4)
par(mfrow=c(2,2))
plot(y4);ad.test(y4$residuals)
dev.off()

zds2=ifelse(zds>19,zds,NA)
plot(zds2,d,col='green',main='Zung Depression Scale (ZDS) vs Trauma Symptom Inventory (TSI)',pch=15,xlab='ZDS Level of Depression',ylab='TSI Depression T-score')
y5=lm(d~zds2)
abline(y5,col='black',lwd=3)
summary(y5)
par(mfrow=c(2,2))
plot(y5);ad.test(y5$residuals)
dev.off()

zds3=ifelse(zds>44,zds,NA)
plot(zds3,d,col='green',main='Zung Depression Scale (ZDS) vs Trauma Symptom Inventory (TSI)',pch=15,xlab='ZDS Level of Depression',ylab='TSI Depression T-score')
y6=lm(d~zds3)
abline(y6,col='black',lwd=3)
summary(y6)
par(mfrow=c(2,2))
plot(y6);ad.test(y6$residuals)
dev.off()

plot(zds2,dm,col='green',main='Zung Depression Scale (ZDS) vs Minnesota Multiphasic Personality Inventory (MMPI)',pch=15,xlab='ZDS Level of Depression',ylab='MMPI Depression T-score')
y7=lm(dm~zds2)
abline(y7,col='black',lwd=3)
summary(y7)
par(mfrow=c(2,2))
plot(y7);ad.test(y7$residuals)
dev.off()

plot(aa,pt,col='blue',main='Trauma Symptom Inventory (TSI) vs Minnesota Multiphasic Personality Inventory (MMPI)',pch=15,xlab='TSI Anxious Arousal T-score',ylab='MMPI Psychathenia T-score')
y8=lm(pt~aa)
abline(y8,col='black',lwd=3)
summary(y8)
par(mfrow=c(2,2))
plot(y8);ad.test(y8$residuals)
dev.off()

plot(d,dm,col='blue',main='Trauma Symptom Inventory (TSI) vs Minnesota Multiphasic Personality Inventory (MMPI)',pch=15,xlab='TSI Depression T-score',ylab='MMPI Depression T-score')
y9=lm(dm~d)
abline(y9,col='black',lwd=3)
summary(y9)
par(mfrow=c(2,2))
plot(y9);ad.test(y9$residuals)
dev.off()

anxiety=data.frame(bai2,aa,pt)
cor(anxiety,use='pairwise.complete.obs')
cor(anxiety,use='pairwise.complete.obs')^2
depression=data.frame(zds2,d,dm)
cor(depression,use='pairwise.complete.obs')
cor(depression,use='pairwise.complete.obs')^2

#Dave Pairwise Comparison (Validity b/t Tests)
plot(inc,vrin,col="blue",main="Inconsistent Response(TSI) vs. Variable Response Inconsistency(MMPI)",pch=1,xlab="Inconsistent Response(inc)",ylab="Variable Response Inconsistency(vrin)")

par(mfrow=c(2,2))
plot(atr,k,col="blue",main="TSI Atypical Response vs MMPI Defensiveness (K)",pch=2,xlab="ATR T-Score",ylab="K T-score")
plot(atr,l,col="red",main="TSI Atypical Response vs. MMPI Lie",pch=3,xlab="ATR T-score",ylab="L T-score")
plot(atr,s,col="green",main="TSI Atypical Response vs MMPI Superlative Self-Presentation",pch=4,xlab="ATR T-Score",ylab="S T-score")
dev.off()
cor(atr,fakegood,use='pairwise.complete.obs')

par(mfrow=c(1,2))
plot(rl,f,col="darkorange",main="TSI Response Level vs MMPI Infrequency (F)",pch=15,xlab="RL T-Score",xlim=c(30,100),ylab="F T-score")
plot(rl,fb,col="green",main="TSI Response Level vs MMPI F Back",pch=16,xlab="RL T-score",xlim=c(30,100),ylab="FB T-score")
dev.off()
fakebad=data.frame(f,fb)
cor(rl,fakebad,use='pairwise.complete.obs')

retention=ifelse(retention<51,retention,NA)
par(mfrow=c(2,2))
plot(aa,retention,col="rosybrown",main="TSI Anxious Arousal vs TOMM Retention",pch=7,xlab="AA T-score",ylab="Retention")
plot(d,retention,col="seagreen",main="TSI Depression vs TOMM Retention",pch=8,xlab="D T-score",ylab="Retention")
plot(ai,retention,col="thistle",main="TSI Anger/Irritability vs TOMM Retention",pch=9,xlab="AI T-score",ylab="Retention")
plot(ie,retention,col="turquoise",main="TSI Intrusive Experiences vs TOMM Retention",pch=10,xlab="IE T-score",ylab="Retention")
dev.off()
par(mfrow=c(2,2))
plot(da,retention,col="navy",main="TSI Defensive Avoidance vs TOMM Retention",pch=11,xlab="DA T-score",ylab="Retention")
plot(dis,retention,col="greenyellow",main="TSI Dissociation vs TOMM Retention ",pch=12,xlab="DIS T-score",ylab="Retention")
plot(sd,retention,col="purple",main="TSI Sexual Concerns vs TOMM Retention",pch=13,xlab="SC T-score",ylab="Retention")
plot(dsb,retention,col="gold",main="TSI Dysfunctional Sexual Behavior vs TOMM Retention",pch=14, xlab="DSB T-score", ylab="Retention")
dev.off()
par(mfrow=c(1,2))
plot(isr,retention,col="dodgerblue",main="TSI Impaired Self-Reference vs TOMM Retention",pch=15,xlab="ISR T-score",ylab="Retention")
plot(trb,retention,col="firebrick",main="TSI Tension Reduction Behavior vs TOMM Retention",pch=16,xlab="TRB T-score",ylab="Retention")
dev.off()

retention2=ifelse(retention<45,retention,NA)
(length(retention)-sum(is.na(retention2)))/sum(!is.na(retention))

tsiclin=data.frame(aa,d,ai,ie,da,dis,sd,dsb,isr,trb)
cor(retention,tsiclin,use='pairwise.complete.obs')
mmpiclin=data.frame(hs,dm,hy,pd,mf,pa,pt,sc,ma,si)
cor(retention,mmpiclin,use='pairwise.complete.obs')

#Allen Multiple/Simple Regression (Validity w/i MMPI)
names(dat)
fakegood=data.frame(k,l,s)
pairs(fakegood)

y10=lm(k~l);y11=lm(k~s);y12=lm(l~s)
summary(y10);summary(y11);summary(y12)

y13=lm(k~l*s);y14=lm(l~k*s);y15=lm(s~k*l)
summary(y13);summary(y14);summary(y15)

y16=lm(s~k+l)
summary(y16)
par(mfrow=c(2,2))
plot(y16);ad.test(y16$residuals)
dev.off()

resid=ifelse(abs(y16$residuals)<15,y16$residuals,NA)
sum(is.na(resid))
par(mfrow=c(1,2))
plot(y16$fitted,resid);abline(h=0);
qqnorm(resid);qqline(resid)
ad.test(resid)
dev.off()

plot(f,fb,col='purple',main='MMPI "Faking Bad"',pch=19,xlab='F T-score (in first half of test)',ylab='FB T-score (in second half of test)')
y17=lm(f~fb)
abline(y17,col='black',lwd=3)
summary(y17)
par(mfrow=c(2,2))
plot(y17);ad.test(y17$residuals)
dev.off()

resid2=ifelse(abs(y17$residuals)<40,y17$residuals,NA)
sum(is.na(resid2))
par(mfrow=c(1,2))
plot(y17$fitted,resid2);abline(h=0);
qqnorm(resid2);qqline(resid2)
ad.test(resid2)
dev.off()

plot(fitted(y17),resid(y17))
y18=lm(f~fb*I(fb^2))
summary(y18)
par(mfrow=c(2,2))
plot(y18);ad.test(y18$residuals)
dev.off()

resid3=ifelse(abs(y18$residuals)<40,y18$residuals,NA)
sum(is.na(resid3))
par(mfrow=c(1,2))
plot(y18$fitted,resid3);abline(h=0);
qqnorm(resid3);qqline(resid3)
ad.test(resid3)
dev.off()

boxcox(y17)
sf=sqrt(f)
y19=lm(sf~fb)
summary(y19)
par(mfrow=c(2,2))
plot(y19);ad.test(y19$residuals)
dev.off()

dat=read.csv("stt592dat.csv", header=T)
attach(dat)
names(dat)

dat=data.frame(chartn,ptsd,dat[,19:31])
dat=dat[complete.cases(dat),]
dim(dat)
attach(dat)
names(dat)

install.packages('leaps')
library(leaps)

#Logistic Regression

ynull=glm(ptsd~1)
yfull=glm(ptsd~(inc+atr+rl+aa+d+ai+ie+da+dis+sd+dsb+isr+trb)^2,data=dat,family=binomial)
step(ynull,scope=list(lower=ynull,upper=yfull),direction='forward')
#Backward Elimination commented out for run time
#step(yfull,direction='backward')
step(ynull,scope=list(lower=ynull,upper=yfull),direction='both')

y=glm(ptsd~aa*da,family=binomial)
summary(y)

prob=predict(y,type="response")
pred=rep(F,length(ptsd))
pred[prob>.5]=T
acctab=table(pred,ptsd);acctab
accuracy=sum(diag(acctab))/sum(acctab); accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

y2=glm(ptsd~inc+atr+rl+aa+d+ai+ie+da+dis+sd+dsb+isr+trb+inc*da+atr*rl+atr*aa+atr*sd+atr*trb+rl*ai+rl*dis+rl*sd+aa*ie+aa*da+d*ai+ai*isr+ai*trb+ie*dsb+ie*trb+da*dis+da*sd+da*dsb+da*trb+dis*sd+dis*isr,data=dat,family=binomial)
summary(y2)

prob=predict(y2,type="response")
pred=rep(F,length(ptsd))
pred[prob>.5]=T
acctab=table(pred,ptsd);acctab
accuracy=sum(diag(acctab))/sum(acctab); accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity

length(ptsd)*0.8
dat[493,1]
train=(chartn<31464)
test=dat[!train,]
ptest=ptsd[!train]

ytrain=glm(ptsd~aa*da,data=dat,family=binomial,subset=train)
summary(ytrain)

ptrain=predict(ytrain,test,type="response")
pred=rep(F,length(ptest))
pred[ptrain>.5]=T
acctab=table(pred,ptest);acctab
accuracy=sum(diag(acctab))/sum(acctab); accuracy
specificity=acctab[1]/sum(acctab[1:2]);specificity
sensitivity=acctab[4]/sum(acctab[3:4]);sensitivity
