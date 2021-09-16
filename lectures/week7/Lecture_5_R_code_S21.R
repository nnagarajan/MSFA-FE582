#Review-Simple Linear Regression 
library(MASS)
library(ISLR)
#fix(Boston)
names(Boston)
?Boston
head(Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="prediction")
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
#leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
Boston[375,]
detach(Boston)
#Review-Multiple Linear Regression
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
#alternatively
lm.fit1=update(lm.fit,~.-age)
summary(lm.fit1)
par(mfrow=c(1,1))
############################ 
par(mfrow=c(1,1))
fct=function(x){return(x^4*(1-x))}

dd=seq(0,1,0.01)
unlist(lapply(dd,fct))
plot(dd,unlist(lapply(dd,fct)))
############################ 
##Logistic
library(ISLR)
names(Default)
head(Default)
summary(Default)
sapply(Default[,c(3,4)],sd)

#install.packages("aod")
#install.packages("ggplot2")

library(aod)
library(ggplot2)

xtabs(~default+student, data=Default)

plot(Default$balance,Default$income)
library(car) 

scatterplot(income ~ balance | default, data=Default, xlab="Balance", ylab="Income", 
            main="Annual Income & Monthly Credit Card Payments")

boxplot(balance ~ default, data=Default, xlab="Default", ylab="Balance")
boxplot(income ~ default, data=Default, xlab="Default", ylab="Income")

glm.default.balance=glm(default~balance,data=Default,family=binomial)
glm.default.balance
summary(glm.default.balance)

glm.default.student=glm(default~student,data=Default,family=binomial)
glm.default.student
summary(glm.default.student)

glm.default.multi=glm(default~balance+student+income,data=Default,family=binomial)
glm.default.multi
summary(glm.default.multi)

glm.default.multi.refined=glm(default~balance+student,data=Default,family=binomial)
glm.default.multi.refined
summary(glm.default.multi.refined)

############################
glm.fit=glm(default~balance+student+income,data=Default,family=binomial)
summary(glm.fit)
confint(glm.fit)
confint.default(glm.fit)
exp(coef(glm.fit))

newdata1 = with(Default, data.frame(balance=mean(balance),income=mean(income),student=c("Yes","No")))
newdata1$StudentYes = predict(glm.fit,newdata=newdata1,type="response")
newdata1

summary(Default)
newdata2 = with(Default, data.frame(balance=seq(from=0,to=2600,length.out=100),income=mean(income),student=c("Yes","No")))
newdata2

newdata3=cbind(newdata2,predict(glm.fit,newdata=newdata2,type="link",se=TRUE))
newdata3=within(newdata3,{
  PredictedProb = plogis(fit)
  LL=plogis(fit-(1.96*se.fit))
  UL=plogis(fit+(1.96*se.fit))
})

head(newdata3)
ggplot(newdata3, aes(x=balance,y=PredictedProb))+geom_ribbon(aes(ymin=LL,ymax=UL,fill=student),alpha=0.2)+geom_line(aes(colour=student),size=1)

glm.probs=predict(glm.fit,newdata = Default, type="response")
glm.probs[1:10]
dim(Default)
glm.pred=rep("No",dim(Default)[1])
glm.pred[glm.probs>0.5]="Yes"
prediction.glm=cbind(Default,glm.pred)
head(prediction.glm)
colnames(prediction.glm)[5]="default prediction"
head(prediction.glm)
#View(prediction.glm)
contrasts(Default$default)
table(glm.pred,Default$default)
mean(glm.pred == Default$default)
#overall error rate
1-mean(glm.pred == Default$default)
#error among defaulted
228/(228+105)
#sensitivity (percentage of true defaulters identified)
105/(228+105)
#specificity (percentage of true non-defaulters that are correctly identified)
9627/(9627+40)

dim(Default)
training=Default[1:5000,]
test=Default[5001:10000,]
glm.fit=glm(default~balance+student+income,data=training,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,newdata = test, type="response")
glm.probs[1:10]
dim(test)
glm.pred=rep("No",dim(test)[1])
glm.pred[glm.probs>0.5]="Yes"
prediction.glm=cbind(test,glm.pred)
head(prediction.glm)
colnames(prediction.glm)[5]="default prediction"
head(prediction.glm)
#View(prediction.glm)
contrasts(test$default)
table(glm.pred,test$default)
mean(glm.pred == test$default)

##LDA
library(MASS)
lda.fit=lda(default~student+balance+income,data=training)
lda.fit
 plot(lda.fit)
lda.pred=predict(lda.fit,test)
head(lda.pred)
lda.class=lda.pred$class
prediction.lda=cbind(test,lda.class)
colnames(prediction.lda)[5]="default prediction"
#View(prediction.lda)
contrasts(test$default)
table(lda.class ,test$default)
mean(lda.class == test$default)
sum(lda.pred$posterior [ ,1] >=.5)
sum(lda.pred$posterior [,1]<.5)
lda.pred$posterior [1:20 ,1]
lda.class [1:20]
sum(lda.pred$posterior[,1]>.9)
#QDA
qda.fit=qda(default~student+balance+income,data=training)
qda.fit
qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
prediction.qda=cbind(test,qda.class)
colnames(prediction.qda)[5]="default prediction"
#View(prediction.lda)
contrasts(test$default)
table(qda.class ,test$default)
mean(qda.class == test$default)
#KNN
test.x=cbind(test$student,test$balance,test$income)
training.x=cbind(training$student,training$balance,training$income)
library(class)
knn.pred=knn(training.x,test.x,training$default,k=5)
prediction.knn=cbind(test,knn.pred)
colnames(prediction.knn)[5]="default prediction"
contrasts(test$default)
table(knn.pred ,test$default)
mean(knn.pred == test$default)
#compare results
comparison=cbind(test,glm.pred,lda.class,qda.class,knn.pred)
head(comparison)