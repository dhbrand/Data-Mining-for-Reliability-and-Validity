dat=read.csv('stt592dat.csv',header=T)
attach(dat)
names(dat)

ldeployn=c(441,149,132,19,139,13)
slices=ldeployn
labels=c('Iraq - ','Afg - ','Both - ','Homebased - ','Unspecified - ','Other - ')
percent=round(slices/893*100,digits=2)
labels=paste(labels,percent)
labels=paste(labels,'%')
pie(slices,labels=labels,col=rainbow(6),main='Location Deployed')

hist(age,col=rainbow(10),main='Age',xlab='Age')
summary(age);sd(age,na.rm=T)

boxplot(education,col='green',main='Education',ylab='Level')
summary(education);sd(education,na.rm=T)

table(gender)
gendern=c(850,41,2)
barplot(gendern,col=rainbow(3),main='Gender',names.arg=c('Male','Female','Unspecified'))

table(ethnicity)
ethnicityn=c(634,50,51,8,3,5,142)
barplot(ethnicityn,col=rainbow(7),main='Ethnicity')
legend(c('Caucasian','African American','Hispanic','Asian American','American Indian','Pacific Islander','Unspecified'),cex=0.8,fill=c(rainbow(8)),inset=0,x='topright')

hist(retention,col='purple',main='Test of Memory Malingering',xlab='TOMM Retention')
summary(retention);sd(retention,na.rm=T)
hist(bai,col='blue',main='Beck Anxiety Inventory',xlab='Anxiety Level')
summary(bai);sd(bai,na.rm=T)
hist(aa,col='blue',main='TSI Anxious Arousal',xlab='AA T-score')
summary(aa);sd(aa,na.rm=T)
hist(zds,col='green',main='Zung Depression Scale',xlab='Level of Depression')
summary(zds);sd(zds,na.rm=T)
hist(d,col='green',main='TSI Depression',xlab='D T-score')
summary(d);sd(d,na.rm=T)
