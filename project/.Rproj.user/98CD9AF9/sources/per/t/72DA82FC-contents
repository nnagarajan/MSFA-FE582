---
  title: "FE-582: Financial Data Science: Predicting Market returns during pandemic"
author: "Daniel Bachalis, Gurjivan Kalkat, Naveen Nagarajan"
date: 'Due: May 16, 2021'
output:
  html_document: default
pdf_document: default
editor_options: 
  markdown: 
  wrap: 72
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_knit$set(root.dir = "D:/Dan/Stevens/Financial Data Science/Project")
library(tidyverse)
library(readxl)
options(scipen=999)
library('gdata')
library('ggplot2')
library('plyr')
library('doBy')
library('readxl') 
library('pastecs')
library(ggpubr)
library(ISLR)
library(splines)
library(gam)
library(tree)
library(randomForest)
library(corrplot)
library(GGally)
library(quantmod)
library(fBasics)
library(xts)
library(MASS)
library(class)
library(caret)
library(e1071)
library(vars)
library(marima)
```

\#\#Exploratory Data Analysis (EDA)

\#\#\#Nasdaq, Gold and Oil (Dan) \* Load in datasets for Nasdaq (NDX),
Gold, Oil futures and Covid data \* Explore and compare visually, look
for outliers and anything interesting

```{r}
# Load in datasets
Covid_ndx_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 1)
SPX_DOW_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 2)
MACRO_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 3)

# Format the dates
Covid_ndx_DS$Date <- as.Date(as.character(Covid_ndx_DS$Date), format = "%Y-%m-%d")
SPX_DOW_DS$Date <- as.Date(as.character(SPX_DOW_DS$Date), format = "%Y-%m-%d")
MACRO_DS<- rbind(MACRO_DS,c('2021-03-02',0,0,0))
MACRO_DS$Date <- as.Date(as.character(MACRO_DS$Date), format = "%Y-%m-%d")
MACRO_DS$GDP<-as.numeric(MACRO_DS$GDP)
MACRO_DS$Unemployment<-as.numeric(MACRO_DS$Unemployment)

MACRO_DS<- MACRO_DS %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% fill('GDP') %>% fill('Unemployment') 



# Combine datasets
covid.full.ds <- right_join(Covid_ndx_DS, SPX_DOW_DS, by = "Date")
covid.full.ds <- left_join(covid.full.ds,MACRO_DS,by = "Date")

# replacing NA with 0
covid.full.ds<- covid.full.ds %>%
  mutate(Cases = if_else(is.na(Cases), 0, Cases)) %>%
  mutate(Deaths = if_else(is.na(Deaths), 0, Deaths))

summary(covid.full.ds %>% dplyr::select(Date:Unemployment))

# Check the dataset
head(covid.full.ds)
tail(covid.full.ds)

#Correlation matrix
ggcorr(covid.full.ds %>% dplyr::select(Date:Cases,Deaths:Unemployment), method = c("everything", "pearson"))


covid.full.ds<- covid.full.ds %>% dplyr::mutate(rnum = row_number())

covid.full.ds<-covid.full.ds %>% arrange(Date)


# Timeseries analysis
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
                         Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
                         NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>% 
  pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
  dplyr::select(Date,variable,values) %>%
  ggplot(mapping = aes(x=Date,y=values,color=variable)) + 
  geom_line(position = position_dodge(width=0.5),size=1) + 
  scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+ 
  theme(legend.position="bottom")


# calculate retursn and diff
covid.full.ds<-data.frame(covid.full.ds %>% arrange(Date) %>% dplyr::select(Date:rnum),
                          c(0,diff(covid.full.ds$Cases)) ,
                          c(0,diff(covid.full.ds$Deaths)),
                          c(0,diff(log(covid.full.ds$SPX))),
                          c(0,diff(log(covid.full.ds$Bitcoin))),
                          c(0,diff(log(covid.full.ds$DJIA))),
                          c(0,diff(log(covid.full.ds$NDX))),
                          c(0,diff(log(covid.full.ds$TLT))),
                          c(0,diff(log(covid.full.ds$Gold))),
                          c(0,diff(log(covid.full.ds$Oil))),
                          0,
                          0,
                          0,
                          0,
                          0,
                          0,
                          0
) %>% arrange(Date)
names(covid.full.ds)[16]<-"Cases.diff"
names(covid.full.ds)[17]<-"Deaths.diff"
names(covid.full.ds)[18]<-"SPX.log.ret"
names(covid.full.ds)[19]<-"Bitcoin.log.ret"
names(covid.full.ds)[20]<-"DJIA.log.ret"
names(covid.full.ds)[21]<-"NDX.log.ret"
names(covid.full.ds)[22]<-"TLT.log.ret"
names(covid.full.ds)[23]<-"Gold.log.ret"
names(covid.full.ds)[24]<-"Oil.log.ret"
names(covid.full.ds)[25]<-"SPX.ret"
names(covid.full.ds)[26]<-"Bitcoin.ret"
names(covid.full.ds)[27]<-"DJIA.ret"
names(covid.full.ds)[28]<-"NDX.ret"
names(covid.full.ds)[29]<-"TLT.ret"
names(covid.full.ds)[30]<-"Gold.ret"
names(covid.full.ds)[31]<-"Oil.ret"


covid.full.ds$SPX.ret<-exp(covid.full.ds$SPX.log.ret) -1
covid.full.ds$Bitcoin.ret<-exp(covid.full.ds$Bitcoin.log.ret) -1
covid.full.ds$DJIA.ret<-exp(covid.full.ds$DJIA.log.ret) -1
covid.full.ds$NDX.ret<-exp(covid.full.ds$NDX.log.ret) -1
covid.full.ds$TLT.ret<-exp(covid.full.ds$TLT.log.ret) -1
covid.full.ds$Gold.ret<-exp(covid.full.ds$Gold.log.ret) -1
covid.full.ds$Oil.ret<-exp(covid.full.ds$Oil.log.ret) -1


covid.full.ds$SPX.trend<-if_else(covid.full.ds$SPX.ret>0,"Up","Down")
covid.full.ds$Bitcoin.trend<-if_else(covid.full.ds$Bitcoin.ret>0,"Up","Down")
covid.full.ds$DJIA.trend<-if_else(covid.full.ds$DJIA.ret>0,"Up","Down")
covid.full.ds$NDX.trend<-if_else(covid.full.ds$NDX.ret>0,"Up","Down")
covid.full.ds$TLT.trend<-if_else(covid.full.ds$TLT.ret>0,"Up","Down")
covid.full.ds$Gold.trend<-if_else(covid.full.ds$Gold.ret>0,"Up","Down")
covid.full.ds$Oil.trend<-if_else(covid.full.ds$Oil.ret>0,"Up","Down")

covid.full.ds$SPX.trend<-as.factor(covid.full.ds$SPX.trend)
covid.full.ds$Bitcoin.trend<-as.factor(covid.full.ds$Bitcoin.trend)
covid.full.ds$DJIA.trend<-as.factor(covid.full.ds$DJIA.trend)
covid.full.ds$NDX.trend<-as.factor(covid.full.ds$NDX.trend)
covid.full.ds$TLT.trend<-as.factor(covid.full.ds$TLT.trend)
covid.full.ds$Gold.trend<-as.factor(covid.full.ds$Gold.trend)
covid.full.ds$Oil.trend<-as.factor(covid.full.ds$Oil.trend)


covid.full.ds$SPX.trend01<-if_else(covid.full.ds$SPX.ret>0,1,0)
covid.full.ds$Bitcoin.trend01<-if_else(covid.full.ds$Bitcoin.ret>0,1,0)
covid.full.ds$DJIA.trend01<-if_else(covid.full.ds$DJIA.ret>0,1,0)
covid.full.ds$NDX.trend01<-if_else(covid.full.ds$NDX.ret>0,1,0)
covid.full.ds$TLT.trend01<-if_else(covid.full.ds$TLT.ret>0,1,0)
covid.full.ds$Gold.trend01<-if_else(covid.full.ds$Gold.ret>0,1,0)
covid.full.ds$Oil.trend01<-if_else(covid.full.ds$Oil.ret>0,1,0)

covid.full.ds$SPX.trend01<-as.factor(covid.full.ds$SPX.trend01)
covid.full.ds$Bitcoin.trend01<-as.factor(covid.full.ds$Bitcoin.trend01)
covid.full.ds$DJIA.trend01<-as.factor(covid.full.ds$DJIA.trend01)
covid.full.ds$NDX.trend01<-as.factor(covid.full.ds$NDX.trend01)
covid.full.ds$TLT.trend01<-as.factor(covid.full.ds$TLT.trend01)
covid.full.ds$Gold.trend01<-as.factor(covid.full.ds$Gold.trend01)
covid.full.ds$Oil.trend01<-as.factor(covid.full.ds$Oil.trend01)

summary(covid.full.ds)
```

More Exploratory analsyis

```{r}

#Time Series for TSA and COVID
ggplot(data=covid.full.ds, aes(x=Date, y=TSA)) +
  geom_line()
ggplot(data=covid.full.ds, aes(x=Cases, y=Deaths)) +
  geom_line()
ggplot(data=covid.full.ds, aes(x=Cases, y=Rate)) +
  geom_line()
ggplot(data=covid.full.ds, aes(x=Cases, y=TSA)) +
  geom_line()
ggplot(data=covid.full.ds, aes(x=Date, y=Deaths, na.rm=TRUE)) +
  geom_line()
# Visualize and explore the datasets
ggplot(covid.full.ds, aes(x = Date, y = Deaths, na.rm = TRUE)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Date, y = NDX)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Date, y = Gold)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Date, y = Oil)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Cases, y = NDX)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Cases, y = Gold)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Cases, y = Oil)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Deaths, y = NDX)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Deaths, y = Gold)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Deaths, y = Oil)) +
  geom_line()
ggplot(covid.full.ds, aes(x = Cases, y = Deaths)) +
  geom_line()

#More plots

# SPX Plot
plot1<-ggplot(covid.full.ds, aes(x = Date, y = Cases)) +
  geom_line() 
plot2<-ggplot(covid.full.ds, aes(x = Date, y = Deaths)) +
  geom_line() 
plot3<-ggplot(covid.full.ds, aes(x = Date, y = SPX)) +
  geom_line()
plot4<-ggplot(covid.full.ds, aes(x = Date, y = Bitcoin)) +
  geom_line()
plot5<-ggplot(covid.full.ds, aes(x = Date, y = DJIA)) +
  geom_line()
plot6 <- covid.full.ds %>% dplyr::filter(GDP>0) %>% ggplot(aes(x = Date, y = GDP)) +
  geom_line()
plot7 <- covid.full.ds %>% dplyr::filter(Unemployment>0) %>% ggplot(aes(x = Date, y = Unemployment)) +
  geom_line()

ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,nrow = 4, ncol=2)


plot8<-ggplot(covid.full.ds, aes(x = Cases, y = SPX, na.rm = TRUE)) +
  geom_line() 
plot9<-ggplot(covid.full.ds, aes(x = Cases, y = Bitcoin, na.rm = TRUE)) +
  geom_line() 
plot10<-ggplot(covid.full.ds, aes(x = Cases, y = DJIA, na.rm = TRUE)) +
  geom_line() 
plot11<- covid.full.ds %>% dplyr::filter(GDP>0) %>% ggplot( aes(x = Cases, y = GDP, na.rm = TRUE)) +
  geom_line() 
plot12<- covid.full.ds %>% dplyr::filter(GDP>0) %>% ggplot( aes(x = Cases, y = Unemployment, na.rm = TRUE)) +
  geom_line() 
ggarrange(plot8,plot9,plot10,plot11,plot12,nrow = 3, ncol=2)

plot13<-ggplot(covid.full.ds, aes(x = Deaths, y = SPX, na.rm = TRUE)) +
  geom_line() 
plot14<-ggplot(covid.full.ds, aes(x = Deaths, y = Bitcoin, na.rm = TRUE)) +
  geom_line() 
plot15<-ggplot(covid.full.ds, aes(x = Deaths, y = DJIA, na.rm = TRUE)) +
  geom_line() 
plot16<- covid.full.ds %>% dplyr::filter(GDP>0) %>% ggplot( aes(x = Deaths, y = GDP, na.rm = TRUE)) +
  geom_line() 
plot17<- covid.full.ds %>% dplyr::filter(GDP>0) %>% ggplot( aes(x = Deaths, y = Unemployment, na.rm = TRUE)) +
  geom_line() 
ggarrange(plot13,plot14,plot15,plot16,plot17,nrow = 3, ncol=2)

```


**LDA, QDA, KNN trained with all data, TSA, Gold, TLT, OIL, Unemployment, Covid Cases, Covid Deaths and GDP as predictors**
  ```{r}
# Create training and test datases based on all data

# 70:30, Test:Train datasets
dt <- sort(sample(nrow(allData), nrow(allData) * .7))
train <- covid.full.ds[dt, ]
test <- covid.full.ds[-dt, ]

# Forecasting datasets (train on all but final month, use final month as test)
#dt <- 1:21
#train <- covid.full.ds[-dt, ]
#test <- covid.full.ds[dt,]

# Predicting NDX
print("NDX")

# LDA
print("LDA")
ldaNDX <- lda(NDX.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
ldaNDX
ldapredictionNDX <- predict(ldaNDX, newdata = test, type = "response")
confusionMatrix(ldapredictionNDX$class, as.factor(test$NDX.trend01))

# QDA
print("QDA")
qdaNDX <- qda(NDX.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
qdaNDX
qdapredictionNDX <- predict(qdaNDX, newdata = test, type = "response")
confusionMatrix(qdapredictionNDX$class, as.factor(test$NDX.trend01))

# KNN using k=1
print("KNN1")
knnNDX1 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$NDX.trend01, k = 1)
knnNDX1
confusionMatrix(knnNDX1, as.factor(test$NDX.trend01))

# KNN using k=100
print("KNN100")
knnNDX100 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$NDX.trend01, k = 100)
knnNDX100
confusionMatrix(knnNDX100, as.factor(test$NDX.trend01))

# KNN using k=5
print("KNN5")
knnNDX5 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$NDX.trend01, k = 1)
knnNDX5
confusionMatrix(knnNDX5, as.factor(test$NDX.trend01))

```

```{r}
# Predicting SPX
print("SPX")

# LDA
print("LDA")
ldaSPX <- lda(SPX.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
ldaSPX
ldapredictionSPX <- predict(ldaSPX, newdata = test, type = "response")
confusionMatrix(ldapredictionSPX$class, as.factor(test$SPX.trend01))
ldaSPX
#QDA
print("QDA")
qdaSPX <- qda(SPX.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
qdaSPX
qdapredictionSPX <- predict(qdaSPX, newdata = test, type = "response")
confusionMatrix(ldapredictionSPX$class, as.factor(test$SPX.trend01))

# KNN using k=1
print("KNN1")
knnSPX1 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$SPX.trend01, k = 1)
knnSPX1
confusionMatrix(knnSPX1, as.factor(test$SPX.trend01))

# KNN using k=100
print("KNN100")
knnSPX100 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$SPX.trend01, k = 100)
knnSPX100
confusionMatrix(knnSPX100, as.factor(test$SPX.trend01))

# KNN using k=5
print("KNN5")
knnSPX5 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$SPX.trend01, k = 100)
knnSPX5
confusionMatrix(knnSPX5, as.factor(test$SPX.trend01))
```

```{r}
# Predicting DJIA
print("DJIA")

# LDA
print("LDA")
ldaDJIA <- lda(DJIA.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
ldaDJIA
ldapredictionDJIA <- predict(ldaDJIA, newdata = test, type = "response")
confusionMatrix(ldapredictionDJIA$class, as.factor(test$DJIA.trend01))

# QDA
print("QDA")
qdaDJIA <- qda(DJIA.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
qdaDJIA
qdapredictionDJIA <- predict(qdaDJIA, newdata = test, type = "response")
confusionMatrix(qdapredictionDJIA$class, as.factor(test$DJIA.trend01))

# KNN using k=1
print("KNN1")
knnDJIA1 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$DJIA.trend01, k = 1)
knnDJIA1
confusionMatrix(knnDJIA1, as.factor(test$DJIA.trend01))

# KNN using k=100
print("KNN100")
knnDJIA100 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$DJIA.trend01, k = 100)
knnDJIA100
confusionMatrix(knnDJIA100, as.factor(test$DJIA.trend01))

# KNN using k=5
print("KNN5")
knnDJIA5 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$DJIA.trend01, k = 100)
knnDJIA5
confusionMatrix(knnDJIA5, as.factor(test$DJIA.trend01))
```

```{r}
# Predicting BTC
print("BTC")

# LDA
print("LDA")
ldaBTC <- lda(DJIA.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
ldaBTC
ldapredictionBTC <- predict(ldaBTC, newdata = test, type = "response")
confusionMatrix(ldapredictionBTC$class, as.factor(test$Bitcoin.trend01))

# QDA
print("QDA")
qdaBTC <- qda(Bitcoin.trend01 ~ TSA + Gold + TLT + Oil + Unemployment + Cases + Deaths + GDP, data = train)
qdaBTC
qdapredictionBTC <- predict(qdaBTC, newdata = test, type = "response")
confusionMatrix(qdapredictionBTC$class, as.factor(test$Bitcoin.trend01))

# KNN using k=1
print("KNN1")
knnBTC1 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$Bitcoin.trend01, k = 1)
knnBTC1
confusionMatrix(knnBTC1, as.factor(test$Bitcoin.trend01))

# KNN using k=100
print("KNN100")
knnBTC100 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$Bitcoin.trend01, k = 1)
knnBTC100
confusionMatrix(knnBTC100, as.factor(test$Bitcoin.trend01))

# KNN using k=5
print("KNN5")
knnBTC5
knnBTC5 <- knn(data.frame(train[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), data.frame(test[, c("TSA", "Gold", "TLT", "Oil", "Unemployment", "Cases", "Deaths", "GDP")]), train$Bitcoin.trend01, k = 100)
confusionMatrix(knnBTC5, as.factor(test$Bitcoin.trend01))
```

```{r}

#Split the Data into Training and Testing
Training<-subset(covid.full.ds, Date <"2021-02-01")
Testing<-subset(covid.full.ds, Date >"2021-01-31")
Training2<-covid.full.ds %>% sample_frac(.7)
Testing2<-covid.full.ds %>% sample_frac(.3)

#NDX Model
NDXGAM<-gam(NDX.ret ~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training)
summary(NDXGAM)
coef(NDXGAM)
plot.Gam(NDXGAM)
NDXGAMprediction<-predict(NDXGAM,Testing,type="response")
NDXGAMpredictionType<- ifelse(NDXGAMprediction > 0, 1, 0)
NDXGAMactualType<-ifelse(Testing$NDX.ret > 0, 1, 0)
table(NDXGAMpredictionType, NDXGAMactualType)


#NDX model using 70/30% Split
NDXGAM2<-gam(NDX.ret ~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training2)
summary(NDXGAM2)
coef(NDXGAM2)
plot.Gam(NDXGAM2,residuals = TRUE)
NDXGAMprediction2<-predict(NDXGAM2,Testing2, type="response")
NDXGAMpredictionType2<- ifelse(NDXGAMprediction2 > 0, 1, 0)
NDXGAMactualType2<-ifelse(Testing2$NDX.ret > 0, 1, 0)
table(NDXGAMpredictionType2, NDXGAMactualType2)


#DJIA Model
DJIAGAM<-gam(DJIA.ret~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training)
summary(DJIAGAM)
coef(DJIAGAM)
plot.Gam(DJIAGAM,se=TRUE)
DJIAGAMprediction<-predict(DJIAGAM,Testing, type="response")
DJIAGAMpredictionType<- ifelse(DJIAGAMprediction > 0, 1, 0)
DJIAGAMactualType<-ifelse(Testing$DJIA.ret > 0, 1, 0)
table(DJIAGAMpredictionType, DJIAGAMactualType)




#DJIA Model using 70/30% Split
DJIAGAM2<-gam(DJIA.ret~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training2)
summary(DJIAGAM2)
coef(DJIAGAM2)
plot.Gam(DJIAGAM2,residuals = TRUE)
DJIAGAMactualType2<-ifelse(Testing2$DJIA.ret > 0, 1, 0)
DJIAGAMprediction2<-predict(DJIAGAM2,Testing2,type="response")
DJIAGAMpredictionType2<- ifelse(DJIAGAMprediction2 > 0, 1, 0)
table(DJIAGAMpredictionType2, DJIAGAMactualType2)


#SPX Covid Model
SPXGAM<-gam(SPX.ret~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training)
summary(SPXGAM)
coef(SPXGAM)
plot.Gam(SPXGAM,se=TRUE)
SPXGAMprediction<-predict(SPXGAM,Testing,type="response")
SPXGAMpredictionType<- ifelse(SPXGAMprediction > 0, 1, 0)
SPXGAMactualType<-ifelse(Testing$SPX.ret > 0, 1, 0)
table(SPXGAMpredictionType, SPXGAMactualType)


#SPX Model using 70/30% Split
SPXGAM2<-gam(SPX.ret~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training2)
summary(SPXGAM2)
coef(SPXGAM2)
plot.Gam(SPXGAM2,residuals = TRUE)
SPXGAMprediction2<-predict(SPXGAM2,Testing2,type="response")
SPXGAMpredictionType2<- ifelse(SPXGAMprediction2 > 0, 1, 0)
SPXGAMactualType2<-ifelse(Testing2$SPX.ret > 0, 1, 0)
table(SPXGAMpredictionType2, SPXGAMactualType2)


#Bitcoin Model
BTCGAM<-gam(Bitcoin.ret ~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training)
summary(BTCGAM)
coef(BTCGAM)
plot.Gam(BTCGAM,residuals = TRUE)
BTCGAMpredicition<-predict(BTCGAM,Testing,type="response")
BTCGAMpredictionType<- ifelse(BTCGAMpredicition > 0, 1, 0)
BTCGAMactualType<-ifelse(Testing$Bitcoin.ret > 0, 1, 0)
table(BTCGAMpredictionType, BTCGAMactualType)



#Bitcoin Model using 70/30% Split
BTCGAM2<-gam(Bitcoin.ret ~ s(TLT)+s(Gold)+s(Oil)+s(TSA)+s(Cases)+s(Rate)+s(Deaths)+s(GDP)+s(Unemployment), data = Training2)
summary(BTCGAM2)
coef(BTCGAM2)
plot.Gam(BTCGAM2,residuals = TRUE)
BTCGAMpredicition2<-predict(BTCGAM2,Testing2,type="response")
BTCGAMpredictionType2<- ifelse(BTCGAMpredicition2 > 0, 1, 0)
BTCGAMactualType2<-ifelse(Testing2$Bitcoin.ret > 0, 1, 0)
table(BTCGAMpredictionType2, BTCGAMactualType2)

```


```{r}

#Split into test and train
covid.full.ds.train <- covid.full.ds %>% dplyr::filter(Date<as.Date("2021-02-01"), Date>as.Date("2019-01-02")) 
covid.full.ds.test  <- anti_join(covid.full.ds  %>% dplyr::filter(Date>as.Date("2019-01-02")) , covid.full.ds.train, by = 'rnum')

#Filter required data for forecast
covid.full.ds.train.1<- covid.full.ds.train %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
covid.full.ds.test.1<- covid.full.ds.test %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)


#Split into test and train based on sampling
covid.full.ds.train <- covid.full.ds %>% dplyr::filter( Date>as.Date("2019-01-02"))  %>% sample_frac(.70)
covid.full.ds.test  <- anti_join(covid.full.ds  %>% dplyr::filter(Date>as.Date("2019-01-02")) , covid.full.ds.train, by = 'rnum')

covid.full.ds.train.2<- covid.full.ds.train %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
covid.full.ds.test.2<- covid.full.ds.test %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)



#RandomForest

##SPX

set.seed(3)
rf.covid.spx<-randomForest(SPX.trend~.,data=covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend), 
                           mtry=3, tree=2000,importance=TRUE)
yhat.rf.spx<-predict(rf.covid.spx, newdata = covid.full.ds.test.2,positive="Down")
confusionMatrix(yhat.rf.spx,covid.full.ds.test.2$SPX.trend)
importance(rf.covid.spx)
varImpPlot(rf.covid.spx)

##Bitcoin
rf.covid.bitcoin<-randomForest(Bitcoin.trend~.,data=covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,Bitcoin.trend), 
                               mtry=5, tree=2000,importance=TRUE)
yhat.rf.bitcoin<-predict(rf.covid.bitcoin, newdata = covid.full.ds.test.2,positive="Down")
confusionMatrix(yhat.rf.bitcoin,covid.full.ds.test.2$Bitcoin.trend)
importance(rf.covid.bitcoin)
varImpPlot(rf.covid.bitcoin)

#DJIA
rf.covid.djia<-randomForest(DJIA.trend~.,data=covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,DJIA.trend), 
                            mtry=5, tree=2000,importance=TRUE)
yhat.rf.djia<-predict(rf.covid.djia, newdata = covid.full.ds.test.2,positive="Down")
confusionMatrix(yhat.rf.djia,covid.full.ds.test.2$DJIA.trend)
importance(rf.covid.djia)
varImpPlot(rf.covid.djia)


#NDX
rf.covid.ndx<-randomForest(NDX.trend~.,data=covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,NDX.trend), 
                           mtry=5, tree=5000,importance=TRUE)
yhat.rf.ndx<-predict(rf.covid.ndx, newdata = covid.full.ds.test.2)
confusionMatrix(yhat.rf.ndx,covid.full.ds.test.2$NDX.trend,positive="Down")
importance(rf.covid.ndx)
varImpPlot(rf.covid.ndx)




#xgboost

#SPX
set.seed(3)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix, 
                           label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), 
                           max_depth = 10, 
                           eta = 0.2,
                           nrounds = 100, 
                           objective = "binary:logistic",
                           booster="gbtree", 
                           min_child_weight=1,
                           gamma = 0.5, 
                           sampling_method= "uniform",
                           subsample=1, 
                           tree_method="exact", 
                           grow_policy="lossguide",
                           num_parallel_tree=2,
                           nthread=2, 
                           max_delta_step=2,
                           predictor="cpu_predictor")
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend,positive="Down")
yhat.xgboost.spx.imp.raw<-xgb.importance(model = xgboost.covid.spx)
xgb.plot.importance(importance_matrix = yhat.xgboost.spx.imp.raw)


#BTC
xgboost.covid.btc<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix, 
                           label = if_else(covid.full.ds.train.2$Bitcoin.trend=="Up",1,0), 
                           max_depth = 10, 
                           eta = 0.2,
                           nrounds = 100, 
                           objective = "binary:logistic",
                           booster="gbtree", 
                           min_child_weight=1,
                           gamma = 0.5, 
                           sampling_method= "uniform",
                           subsample=1, 
                           tree_method="exact", 
                           grow_policy="lossguide",
                           num_parallel_tree=2,
                           nthread=2, 
                           max_delta_step=2,
                           predictor="cpu_predictor")
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.5,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend,positive="Down")
yhat.xgboost.btc.imp.raw<-xgb.importance(model = xgboost.covid.btc)
xgb.plot.importance(importance_matrix = yhat.xgboost.btc.imp.raw)


#DJIA
xgboost.covid.djia<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix, 
                            label = if_else(covid.full.ds.train.2$DJIA.trend=="Up",1,0), 
                            max_depth = 10, 
                            eta = 0.2,
                            nrounds = 100, 
                            objective = "binary:logistic",
                            booster="gbtree", 
                            min_child_weight=1,
                            gamma = 0.5, 
                            sampling_method= "uniform",
                            subsample=1, 
                            tree_method="exact", 
                            grow_policy="lossguide",
                            num_parallel_tree=2,
                            nthread=2, 
                            max_delta_step=2,
                            predictor="cpu_predictor")
yhat.xgboost.djia<-predict(xgboost.covid.djia, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.djia>0.5,"Up","Down")),covid.full.ds.test.2$DJIA.trend,positive="Down")
yhat.xgboost.djia.imp.raw<-xgb.importance(model = xgboost.covid.djia)
xgb.plot.importance(importance_matrix = yhat.xgboost.djia.imp.raw)

#NDX
xgboost.covid.ndx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix, 
                           label = if_else(covid.full.ds.train.2$NDX.trend=="Up",1,0), 
                           max_depth = 10, 
                           eta = 0.2,
                           nrounds = 100, 
                           objective = "binary:logistic",
                           booster="gbtree", 
                           min_child_weight=1,
                           gamma = 0.5, 
                           sampling_method= "uniform",
                           subsample=1, 
                           tree_method="exact", 
                           grow_policy="lossguide",
                           num_parallel_tree=2,
                           nthread=2, 
                           max_delta_step=2,
                           predictor="cpu_predictor")
yhat.xgboost.ndx<-predict(xgboost.covid.ndx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.ndx>0.5,"Up","Down")),covid.full.ds.test.2$NDX.trend,positive="Down")
yhat.xgboost.ndx.imp.raw<-xgb.importance(model = xgboost.covid.ndx)
xgb.plot.importance(importance_matrix = yhat.xgboost.ndx.imp.raw)

```



