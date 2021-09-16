library("ggplot2")
library('gdata')
library('dplyr')
library('lubridate')
library('tidyverse')
library('collections')
library('Hmisc')
library('psych')
library('ISLR')
library('class')
library('MASS')
options(scipen = 999)
setwd("Z:\\Desktop\\workspace\\stevens-fa\\FE582WS\\assignments\\assignment-3")


# a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. 
# You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.

auto.ds=read.csv("HW3_S21/Auto.csv")
median.auto.mpg=median(auto.ds$mpg)

auto.ds = auto.ds %>% mutate(mpg01=case_when( mpg>median.auto.mpg ~ 1 , TRUE ~ 0 ))

auto.ds %>% tibble

auto.ds$mpg01<-as.factor(auto.ds$mpg01)
#auto.ds$name<-as.factor(auto.ds$name)
auto.ds$cylinders<-as.factor(auto.ds$cylinders)
auto.ds$origin<-as.factor(auto.ds$origin)

summary(auto.ds)
unique(auto.ds$origin)


# b) Explore the data graphically in order to investigate the association between mpg01 and the other features. 
# Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.


egg::ggarrange(auto.ds %>%  ggplot(mapping = aes(x=cylinders,fill=mpg01)) + geom_bar(position = "dodge") ,
               auto.ds %>%  ggplot(mapping = aes(y=displacement,x=mpg01)) + geom_boxplot() ,
               auto.ds %>%  ggplot(mapping = aes(y=horsepower,x=mpg01)) + geom_boxplot() ,
               auto.ds %>%  ggplot(mapping = aes(y=weight,x=mpg01)) + geom_boxplot() ,
               auto.ds %>%  ggplot(mapping = aes(y=acceleration,x=mpg01)) + geom_boxplot() ,
               auto.ds %>%  ggplot(mapping = aes(x=origin,fill=mpg01)) + geom_bar(position = "dodge") ) 

## From the chart displacement, horsepower, weight, cylinders, origin is closely correlated with the mpg01,  acceleration doesn't as the decision boundary is overlaps.

# c) Split the data into a training set and a test set.
auto.ds = auto.ds %>% mutate(ID=row_number())
#Create training set
auto.ds.train <- auto.ds %>% sample_frac(.70)
#Create test set
auto.ds.test  <- anti_join(auto.ds,auto.ds.train,by='ID')

auto.ds.train<- auto.ds.train %>% dplyr::select(mpg:mpg01)
auto.ds.test <- auto.ds.test %>% dplyr::select(mpg:mpg01)
head(auto.ds.train)
head(auto.ds.test)

auto.ds.train$cylinders<-as.numeric(auto.ds.train$cylinders)
auto.ds.test$cylinders<-as.numeric(auto.ds.test$cylinders)

#d) Perform LDA on the training data in order to predict mpg01 using the variables that
#seemed most associated with mpg01 in b). What is the test error of the model
#obtained?
auto.ds.lda=lda(mpg01~cylinders+horsepower+displacement+weight+origin,data=auto.ds.train)
auto.ds.lda.predict=predict(auto.ds.lda,newdata = auto.ds.test, type="response")
auto.ds.lda.merged = cbind(auto.ds.test,auto.ds.lda.predict$class)
names(auto.ds.lda.merged)[ncol(auto.ds.lda.merged)]<-"pred.mpg01"
cfm.lda=table(auto.ds.lda.merged$mpg01,auto.ds.lda.merged$pred.mpg01)
cfms.lda<-data.frame(cfm.lda["1","1"]/sum(cfm.lda["1",]),
                     cfm.lda["0","0"]/sum(cfm.lda["0",]),
                     1-mean(auto.ds.lda.merged$mpg01==auto.ds.lda.merged$pred.mpg01))
names(cfms.lda)<-c("Sensitivity","Specificity","Error rate")
cfms.lda
cfm.lda

#e) Perform QDA on the training data in order to predict mpg01 using the variables that
#seemed most associated with mpg01 in b). What is the test error of the model
#obtained?
#auto.ds.train$cylinders<-as.numeric(auto.ds.train$cylinders)
#auto.ds.test$test<-as.numeric(auto.ds.test$cylinders)

auto.ds.qda=qda(mpg01~cylinders+horsepower+displacement+weight+origin,data=auto.ds.train)
auto.ds.qda.predict= predict(auto.ds.qda, newdata = auto.ds.test, type="response")
auto.ds.qda.merged = cbind(auto.ds.test,auto.ds.qda.predict$class)
names(auto.ds.qda.merged)[ncol(auto.ds.qda.merged)]<-"pred.mpg01"
cfm.qda=table(auto.ds.qda.merged$mpg01,auto.ds.qda.merged$pred.mpg01)
cfms.qda<-data.frame(cfm.qda["1","1"]/sum(cfm.qda["1",]),
                     cfm.qda["0","0"]/sum(cfm.qda["0",]),
                     1-mean(auto.ds.qda.merged$mpg01==auto.ds.qda.merged$pred.mpg01))
names(cfms.qda)<-c("Sensitivity","Specificity","Error rate")
cfms.qda
cfm.qda

#f) Perform logistic regression on the training data in order to predict mpg01 using the
#variables that seemed most associated with mpg01 in b). What is the test error of the
#model obtained?
auto.ds.glm=glm(mpg01~cylinders+horsepower+displacement+weight+origin,data=auto.ds.train,family = "binomial")
auto.ds.glm.predict= predict(auto.ds.glm, newdata = auto.ds.test, type="response")
auto.ds.glm.predict[auto.ds.glm.predict>=0.5]<-"1"
auto.ds.glm.predict[auto.ds.glm.predict<0.5]<-"0"
auto.ds.glm.merged = cbind(auto.ds.test,auto.ds.glm.predict)
names(auto.ds.glm.merged)[ncol(auto.ds.glm.merged)]<-"pred.mpg01"
cfm.glm=table(auto.ds.glm.merged$mpg01,auto.ds.glm.merged$pred.mpg01)
cfms.glm<-data.frame(cfm.glm["1","1"]/sum(cfm.glm["1",]),
                     cfm.glm["0","0"]/sum(cfm.glm["0",]),
                     1-mean(auto.ds.glm.merged$mpg01==auto.ds.glm.merged$pred.mpg01))
names(cfms.glm)<-c("Sensitivity","Specificity","Error rate")
cfms.glm
cfm.glm

# g) Perform KNN on the training data, with several values of K, in order to predict mpg01.
# Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?

#knn : 1
auto.ds.knn.predict=knn(auto.ds.train %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix, 
                auto.ds.test %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix ,
                auto.ds.train$mpg01 ,k=1 )
auto.ds.knn.merged = cbind(auto.ds.test,auto.ds.knn.predict)
names(auto.ds.knn.merged)[ncol(auto.ds.knn.merged)]<-"pred.mpg01"
cfm.knn=table(auto.ds.knn.merged$mpg01,auto.ds.knn.merged$pred.mpg01)
cfms.knn<-data.frame(cfm.knn["1","1"]/sum(cfm.knn["1",]),
                     cfm.knn["0","0"]/sum(cfm.knn["0",]),
                     1-mean(auto.ds.knn.merged$mpg01==auto.ds.knn.merged$pred.mpg01))
names(cfms.knn)<-c("Sensitivity","Specificity","Error rate")
cfms.knn
cfm.knn

#knn : 2

auto.ds.knn.predict.2=knn(auto.ds.train %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix, 
                        auto.ds.test %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix ,
                        auto.ds.train$mpg01 ,k=2 )
auto.ds.knn.merged.2 = cbind(auto.ds.test,auto.ds.knn.predict.2)
names(auto.ds.knn.merged.2)[ncol(auto.ds.knn.merged.2)]<-"pred.mpg01"
cfm.knn.2=table(auto.ds.knn.merged.2$mpg01,auto.ds.knn.merged.2$pred.mpg01)
cfms.knn.2<-data.frame(cfm.knn.2["1","1"]/sum(cfm.knn.2["1",]),
                     cfm.knn.2["0","0"]/sum(cfm.knn.2["0",]),
                     1-mean(auto.ds.knn.merged.2$mpg01==auto.ds.knn.merged.2$pred.mpg01))
names(cfms.knn.2)<-c("Sensitivity","Specificity","Error rate")
cfms.knn.2
cfm.knn.2

#knn : 3

auto.ds.knn.predict.3=knn(auto.ds.train %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix, 
                          auto.ds.test %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix ,
                          auto.ds.train$mpg01 ,k=3 )
auto.ds.knn.merged.3 = cbind(auto.ds.test,auto.ds.knn.predict.3)
names(auto.ds.knn.merged.3)[ncol(auto.ds.knn.merged.3)]<-"pred.mpg01"
cfm.knn.3=table(auto.ds.knn.merged.3$mpg01,auto.ds.knn.merged.3$pred.mpg01)
cfms.knn.3<-data.frame(cfm.knn.3["1","1"]/sum(cfm.knn.3["1",]),
                       cfm.knn.3["0","0"]/sum(cfm.knn.3["0",]),
                       1-mean(auto.ds.knn.merged.3$mpg01==auto.ds.knn.merged.3$pred.mpg01))
names(cfms.knn.3)<-c("Sensitivity","Specificity","Error rate")
cfms.knn.3
cfm.knn.3


#knn : 4

auto.ds.knn.predict.4=knn(auto.ds.train %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix, 
                          auto.ds.test %>% dplyr::select(cylinders,horsepower,displacement,weight,origin) %>% data.matrix ,
                          auto.ds.train$mpg01 ,k=3 )
auto.ds.knn.merged.4 = cbind(auto.ds.test,auto.ds.knn.predict.4)
names(auto.ds.knn.merged.4)[ncol(auto.ds.knn.merged.4)]<-"pred.mpg01"
cfm.knn.4=table(auto.ds.knn.merged.4$mpg01,auto.ds.knn.merged.4$pred.mpg01)
cfms.knn.4<-data.frame(cfm.knn.4["1","1"]/sum(cfm.knn.4["1",]),
                       cfm.knn.4["0","0"]/sum(cfm.knn.4["0",]),
                       1-mean(auto.ds.knn.merged.4$mpg01==auto.ds.knn.merged.4$pred.mpg01))
names(cfms.knn.4)<-c("Sensitivity","Specificity","Error rate")
cfms.knn.4
cfm.knn.4


## Knn with k=1 has less error rate compared 
