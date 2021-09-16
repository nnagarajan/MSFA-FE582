library(tidyverse)
options(scipen=999)
library(tree)

setwd("/Users/naveen/Desktop/workspace/stevens-fa/FE582WS/assignments/assignment-4/")

oj.ds<- read.csv("HW4_S21/OJ.csv")

head(oj.ds)
summary(oj.ds)

oj.ds$Purchase<-as.factor(oj.ds$Purchase)
oj.ds$Store7<-as.factor(oj.ds$Store7)
oj.ds$STORE<-as.factor(oj.ds$STORE)
oj.ds$StoreID<-as.factor(oj.ds$StoreID)


# a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.
oj.ds<-oj.ds%>% mutate(RID = row_number())
oj.ds.train <- oj.ds %>% dplyr::slice_sample(n=800)
oj.ds.test  <- anti_join(oj.ds, oj.ds.train, by = 'RID')
oj.ds.train<-oj.ds.train %>% select(-c(RID))
oj.ds.test<-oj.ds.test %>% select(-c(RID))

# b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors. 
# Use the summary() function to produce summary statistics about the tree, and describe the results obtained. 
# What is the training error rate? How many terminal nodes does the tree have?
attach(oj.ds.train)
oj.ds.tree.fit<-tree(Purchase~.,oj.ds.train)
summary(oj.ds.tree.fit)

## Tree has 7 terminal nodes


# c) Type in the name of the tree object in order to get a detailed text output. 
# Pick one of the terminal nodes, and interpret the information displayed.


# d) Create a plot of the tree, and interpret the results.

plot(oj.ds.tree.fit)
text(oj.ds.tree.fit,pretty = 0)

# e) Predict the response on the test data, and produce a confusion matrix comparing the
# test labels to the predicted test labels. What is the test error rate?
oj.ds.test.predict <- predict(oj.ds.tree.fit,newdata = oj.ds.test,type="class")
oj.cm <- table(oj.ds.test.predict,oj.ds.test$Purchase)
1 - ( (oj.cm[1,1] + oj.cm[2,2]) / nrow(oj.ds.test))

# f) Apply the cv.tree() function to the training set in order to determine the optimal tree
# size.

oj.ds.train.cv.tree <- cv.tree(oj.ds.tree.fit)

# g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on
# the y-axis.

ggplot(mapping=aes(oj.ds.train.cv.tree$size, oj.ds.train.cv.tree$dev)) + geom_line() + geom_point() + xlab("size") +ylab("error")





