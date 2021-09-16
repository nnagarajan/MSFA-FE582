library(tidyverse)
options(scipen=999)
library(tree)
library(gbm)
library(class)
setwd("/Users/naveen/Desktop/workspace/stevens-fa/FE582WS/assignments/assignment-4/")


# a) Create a training set consisting of the first 1,000 observations, and a test set consisting of the remaining observations.

caravan.ds<-read.csv("HW4_S21/CARAVAN.csv")
caravan.ds$Purchase<-ifelse(caravan.ds$Purchase=="Yes",1,0)
#bernoulli doens't work unless the Response variable is of class character
caravan.ds$Purchase<-as.character(caravan.ds$Purchase)
summary(caravan.ds)


caravan.ds<-caravan.ds%>% mutate(RID = row_number())
caravan.ds.train <- caravan.ds %>% dplyr::slice_head(n=1000)
caravan.ds.test  <- anti_join(caravan.ds, caravan.ds.train, by = 'RID')
caravan.ds.train<-caravan.ds.train %>% dplyr::select(-c(RID))
caravan.ds.test<-caravan.ds.test %>% dplyr::select(-c(RID))
#knn doesn't work when scaled values are nan
caravan.ds.train.scaled<- data.frame(caravan.ds.train %>% select(-c(Purchase)) %>% scale)
caravan.ds.test.scaled<- data.frame(caravan.ds.test %>% select(-c(Purchase)) %>% scale)
caravan.ds.train.scaled$PVRAAUT<-ifelse(is.nan(caravan.ds.train.scaled$PVRAAUT),0,0)
caravan.ds.train.scaled$AVRAAUT<-ifelse(is.nan(caravan.ds.train.scaled$AVRAAUT),0,0)



# b) Fit a boosting model to the training set with Purchase as the response and the other variables as predictors. 
# Use 1,000 trees, and a shrinkage value of 0.01. Which predictors appear to be the most important?
  
set.seed(5)

boost.caravan.ds.fit<-gbm(Purchase~.,data = caravan.ds.train, distribution = "bernoulli", n.trees = 1000, 
                      shrinkage = 0.01)

boost.caravan.ds.predict<-predict(boost.caravan.ds.fit, newdata = caravan.ds.test,n.trees = 1000, type = "response")

predicted.purchase<-ifelse(boost.caravan.ds.predict>0.2,"1","0")

summary(boost.caravan.ds.fit)

#marginal dependence plot

plot(boost.caravan.ds.fit,i="PPERSAUT")
plot(boost.caravan.ds.fit,i="MKOOPKLA")

#  car policies(PPERSAUT) and Purchasing power class(MKOOPKLA) are far the most important variables.

# c) Use the boosting model to predict the response on the test data. 
# Predict that a person will make a purchase if the estimated probability of purchase is greater than 20 %. Form a confusion matrix. 
# What fraction of the people predicted to make a purchase do in fact make one? How does this compare with the results obtained from applying KNN or logistic regression to this data set?

caravan.ds.test <- data.frame(caravan.ds.test,predicted.purchase)

caravan.ds.test$PurchaseYN<-ifelse(caravan.ds.test$Purchase=="1","Yes","No")
caravan.ds.test$predicted.purchase<-ifelse(caravan.ds.test$predicted.purchase=="1","Yes","No")

caravan.ds.cm<- table(data.frame(caravan.ds.test$PurchaseYN, caravan.ds.test$predicted.purchase))

1 - ( (caravan.ds.cm[1,1] + caravan.ds.cm[2,2]) / nrow(caravan.ds.test))

#knn

set.seed(5)
knn.predicted.purchase<-knn(caravan.ds.train.scaled,  caravan.ds.test.scaled,caravan.ds.train$Purchase,k=1)
caravan.ds.test <- data.frame(caravan.ds.test,knn.predicted.purchase)
caravan.ds.test$knn.predicted.purchase<-ifelse(caravan.ds.test$knn.predicted.purchase=="1","Yes","No")
caravan.ds.knn.cm<- table(data.frame(caravan.ds.test$PurchaseYN, caravan.ds.test$knn.predicted.purchase))
caravan.ds.knn.cm
1 - ( (caravan.ds.knn.cm[1,1] + caravan.ds.knn.cm[2,2]) / nrow(caravan.ds.test))





