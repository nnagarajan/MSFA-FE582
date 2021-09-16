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
library(vars)
library(marima)
read_excel("Clean Project Data-Based on Date.xlsx", 1)

# Load in datasets
Covid_BTC_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 1)
SPX_DOW_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 2)
MACRO_DS <- read_excel("Clean Project Data-Based on Date.xlsx", 3)

# Format the dates
Covid_BTC_DS$Date <- as.Date(as.character(Covid_BTC_DS$Date), format = "%Y-%m-%d")
SPX_DOW_DS$Date <- as.Date(as.character(SPX_DOW_DS$Date), format = "%Y-%m-%d")
MACRO_DS<- rbind(MACRO_DS,c('2021-03-02',0,0,0))
MACRO_DS$Date <- as.Date(as.character(MACRO_DS$Date), format = "%Y-%m-%d")
MACRO_DS$GDP<-as.numeric(MACRO_DS$GDP)
MACRO_DS$Unemployment<-as.numeric(MACRO_DS$Unemployment)

MACRO_DS<- MACRO_DS %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% fill('GDP') %>% fill('Unemployment') 


# Combine datasets
covid.full.ds <- right_join(Covid_BTC_DS, SPX_DOW_DS, by = "Date")
covid.full.ds <- left_join(covid.full.ds,MACRO_DS,by = "Date")

# replacing NA with 0
covid.full.ds<- covid.full.ds %>%
  mutate(Cases = if_else(is.na(Cases), 0, Cases)) %>%
  mutate(Deaths = if_else(is.na(Deaths), 0, Deaths))

summary(covid.full.ds)

# Check the dataset
head(covid.full.ds)
tail(covid.full.ds)

ggcorr(covid.full.ds %>% dplyr::select(Date:Cases,Deaths:Unemployment), method = c("everything", "pearson"))


covid.full.ds<- covid.full.ds %>% dplyr::mutate(rnum = row_number())

covid.full.ds<-covid.full.ds %>% arrange(Date)

# calculate retursn and diff
covid.full.ds<-data.frame(covid.full.ds %>% arrange(Date) %>% dplyr::select(Date:rnum),
                          c(0,diff(covid.full.ds$Cases)) ,
                          c(0,diff(covid.full.ds$Deaths)),
                          c(0,diff(log(covid.full.ds$SPX))),
                          c(0,diff(log(covid.full.ds$Bitcoin))),
                          c(0,diff(log(covid.full.ds$DJIA))),
                          c(0,diff(log(covid.full.ds$NDX)))
                          # ,
                          # c(0,diff(log(covid.full.ds$TLT))),
                          # c(0,diff(log(covid.full.ds$Gold))),
                          # c(0,diff(log(covid.full.ds$Oil)))
) %>% arrange(Date)
names(covid.full.ds)[16]<-"Cases.diff"
names(covid.full.ds)[17]<-"Deaths.diff"
names(covid.full.ds)[18]<-"SPX.ret"
names(covid.full.ds)[19]<-"Bitcoin.ret"
names(covid.full.ds)[20]<-"DJIA.ret"
names(covid.full.ds)[21]<-"NDX.ret"

#################################
names(covid.full.ds)[22]<-"TLT.ret"
names(covid.full.ds)[23]<-"Gold.ret"
names(covid.full.ds)[24]<-"Oil.ret"

summary(covid.full.ds)

#Split into test and train
covid.full.ds.train <- covid.full.ds %>% dplyr::filter(Date<as.Date("2021-01-10"))
covid.full.ds.test  <- anti_join(covid.full.ds, covid.full.ds.train, by = 'rnum')

covid.full.ds.train.1<- covid.full.ds.train %>% dplyr::select(Date:Cases,Deaths:Unemployment,Cases.diff:Deaths.diff)
covid.full.ds.test.1<- covid.full.ds.test %>% dplyr::select(Date:Cases,Deaths:Unemployment,Cases.diff:Deaths.diff)


covid.full.ds.train.2<- covid.full.ds.train %>% dplyr::select(Date,Cases.diff:Deaths.diff,GDP,Unemployment,SPX.ret:Oil.ret) %>% dplyr::filter(Date>as.Date("2019-01-02"))
covid.full.ds.test.2<- covid.full.ds.test %>% dplyr::select(Date,Cases.diff:Deaths.diff,GDP,Unemployment,SPX.ret:Oil.ret) 

covid.full.ds.train.3<- covid.full.ds.train %>% dplyr::select(Date,Cases.diff:Deaths.diff,GDP,Unemployment,Gold, Oil,TSA,SPX.ret:NDX.ret) %>% dplyr::filter(Date>as.Date("2019-01-02"))
covid.full.ds.test.3<- covid.full.ds.test %>% dplyr::select(Date,Cases.diff:Deaths.diff,GDP,Unemployment,Gold, Oil,TSA,SPX.ret:NDX.ret) 

pairs(covid.full.ds.train %>% dplyr::select(Date:Cases,Deaths:Unemployment))
#cor(covid.full.ds.train %>% dplyr::select(Date:Cases,Deaths:Unemployment))

#corrplot(covid.full.ds.train.1 , method="number", use="complete.obs", is.corr=F)

tail(covid.full.ds.test)

#RandomForest

##SPX

set.seed(3)
rf.covid.spx<-randomForest(SPX~.,data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,-c(NDX,DJIA,Bitcoin)), 
                       mtry=4, importance=TRUE)
yhat.rf.spx<-predict(rf.covid.spx, newdata = covid.full.ds.test.1)
mean((yhat.rf.spx-covid.full.ds.test.1$SPX)^2)
importance(rf.covid.spx)
varImpPlot(rf.covid.spx)

##Bitcoin
rf.covid.bitcoin<-randomForest(Bitcoin~.,data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,-c(NDX,DJIA,SPX)), 
                           mtry=4, importance=TRUE)
yhat.rf.bitcoin<-predict(rf.covid.bitcoin, newdata = covid.full.ds.test.1)
mean((yhat.rf.bitcoin-covid.full.ds.test.1$Bitcoin)^2)
importance(rf.covid.bitcoin)
varImpPlot(rf.covid.bitcoin)

#DJIA
rf.covid.djia<-randomForest(DJIA~.,data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,-c(NDX,Bitcoin,SPX)), 
                               mtry=4, importance=TRUE)
yhat.rf.djia<-predict(rf.covid.djia, newdata = covid.full.ds.test.1)
mean((yhat.rf.djia-covid.full.ds.test.1$DJIA)^2)
importance(rf.covid.djia)
varImpPlot(rf.covid.djia)


#NDX
rf.covid.ndx<-randomForest(NDX~.,data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,-c(DJIA,Bitcoin,SPX)), 
                            mtry=4, importance=TRUE)
yhat.rf.ndx<-predict(rf.covid.ndx, newdata = covid.full.ds.test.1)
mean((yhat.rf.ndx-covid.full.ds.test.1$NDX)^2)
importance(rf.covid.ndx)
varImpPlot(rf.covid.ndx)


#Step Function

#SPX

step.covid.fit.spx<- gam(SPX~cut(Cases.diff,breaks=c(-1,1,10000,30000,70000,400000,900000))+
                           cut(Deaths.diff,breaks=c(-1,1,1000,3000,5000,10000))+TSA+TLT+Unemployment+GDP+Gold+Oil, 
                    data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,Cases.diff:Deaths.diff,-c(NDX,DJIA,Bitcoin)))
yhat.step.covid.spx<-predict(step.covid.fit.spx, newdata = covid.full.ds.test.1)
mean((yhat.step.covid.spx-covid.full.ds.test.1$SPX)^2)
par(mfrow=c(1,1))
plot(yhat.step.covid.spx, se=TRUE,col="blue")

#Bitcoin
step.covid.fit.btc<- gam(Bitcoin~cut(Cases.diff,breaks=c(-1,1,10000,30000,70000,400000,900000))+
                           cut(Deaths.diff,breaks=c(-1,1,1000,3000,5000,10000))+TSA+TLT+Unemployment+GDP+Gold+Oil, 
                         data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,Cases.diff:Deaths.diff,-c(NDX,DJIA,SPX)))
yhat.step.covid.btc<-predict(step.covid.fit.btc, newdata = covid.full.ds.test.1)
mean((yhat.step.covid.btc-covid.full.ds.test.1$Bitcoin)^2)
par(mfrow=c(1,1))
plot(yhat.step.covid.btc, se=TRUE,col="blue")

#DJIA
step.covid.fit.djia<- gam(DJIA~cut(Cases.diff,breaks=c(-1,1,10000,30000,70000,400000,900000))+
                           cut(Deaths.diff,breaks=c(-1,1,1000,3000,5000,10000))+TSA+TLT+Unemployment+GDP+Gold+Oil, 
                         data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,Cases.diff:Deaths.diff,-c(NDX,Bitcoin,SPX)))
yhat.step.covid.djia<-predict(step.covid.fit.djia, newdata = covid.full.ds.test.1)
mean((yhat.step.covid.djia-covid.full.ds.test.1$DJIA)^2)
par(mfrow=c(1,1))
plot(yhat.step.covid.djia, se=TRUE,col="blue")

#NDX
step.covid.fit.ndx<- gam(NDX~cut(Cases.diff,breaks=c(-1,1,10000,30000,70000,400000,900000))+
                            cut(Deaths.diff,breaks=c(-1,1,1000,3000,5000,10000))+TSA+TLT+Unemployment+GDP+Gold+Oil, 
                          data=covid.full.ds.train.1 %>% dplyr::select(TSA:Unemployment,Cases.diff:Deaths.diff,-c(DJIA,Bitcoin,SPX)))
yhat.step.covid.ndx<-predict(step.covid.fit.ndx, newdata = covid.full.ds.test.1)
mean((yhat.step.covid.ndx-covid.full.ds.test.1$NDX)^2)
par(mfrow=c(1,1))
plot(yhat.step.covid.djia, se=TRUE,col="blue")




## Time series analysis

covid.full.ds.train.1 %>% dplyr::select(Date,TSA:Unemployment,-c(NDX,DJIA,Bitcoin)) %>% diff()


window(as.data.frame(covid.full.ds),start=as.Date("2021-03-01"),end=as.Date("2021-04-01"))

data.frame(covid.full.ds, -diff(covid.full.ds$Cases) )

acf(covid.full.ds.train %>% dplyr::select(TSA:Unemployment,-c(rnum,Rate)),na.action = na.pass)

window(covid.full.ds)


window(covid.full.ds.train %>% dplyr::select(Date,TSA))

covid.full.ds<- covid.full.ds 


tail(covid.full.ds)

window(ts(covid.full.ds),start=c(2019,1,1),end=c(2020,12,1))

window(ts(covid.full.ds))


set.seed(3)
rf.covid.spx<-randomForest(SPX.ret~.,data=covid.full.ds %>% dplyr::select(Date,TSA,Cases.diff,Deaths.diff,SPX.ret, Oil,Gold, TLT), 
                           mtry=2, importance=TRUE)
yhat.rf.spx<-predict(rf.covid.spx, newdata = covid.full.ds.test)
mean((yhat.rf.spx-covid.full.ds.test$SPX.ret)^2)
importance(rf.covid.spx)
varImpPlot(rf.covid.spx)


acf(covid.full.ds.train %>% dplyr::select(Date,TSA,Cases.diff,Deaths.diff,SPX.ret, Oil,Gold, TLT))

ts_split(covid.full.ds)


#timeseries analysis - Multivariate ARMAX models - pg. 277
tail(covid.full.ds.train.2)
fit<-vars::VAR(ts(covid.full.ds.train.2 %>% dplyr::select(Cases.diff:Oil.ret)),p=5,type='both')


summary(fit)

fit.pr<- stats::predict(fit,n.ahead=34,ci=0.95, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases.diff:Oil.ret))

fit.pr$fcst$Bitcoin.ret

data.frame(fit.pr$fcst$Bitcoin.ret[,1],covid.full.ds.test$Bitcoin.ret)

mean( (fit.pr$fcst$Bitcoin.ret[,1]-covid.full.ds.test$Bitcoin.ret)^2)
mean( (fit.pr$fcst$Bitcoin.ret-covid.full.ds.test$Bitcoin.ret)^2)

0.005644304


?stats::predict

fanchart(fit.pr)

tail(fit.pr)

library(vars)

install.packages("astsa")
x=cbind(astsa::cmort,astsa::tempr,astsa::part)
head(x)
summary(VAR(x, p=1, type='both'))
VARselect(x, lag.max=10, type="both")
summary(fit <- VAR(x, p=2, type="both"))
acf(resid(fit), 52)
serial.test(fit, lags.pt=12, type="PT.adjusted")
(fit.pr = predict(fit, n.ahead = 24, ci = 0.95)) # 4 weeks ahead fanchart(fit.pr) # plot prediction + error
fanchart(fit.pr)

tail(covid.full.ds.train.2)
install.packages("glarma")

YRes=as.matrix(covid.full.ds.train.2 %>% dplyr::select(SPX.ret))
xPredict=as.matrix(covid.full.ds.train.2%>% 
              dplyr::select(Cases.diff,Deaths.diff,GDP,Unemployment,Gold.ret,Oil.ret))

glarmamod<-glarma(YRes,xPredict, 
                  type = "Bin", method = "NR", residuals = "Pearson",
                  )
y <- Asthma[, 1]
X <- as.matrix(Asthma[, 2:16])
glarmamod <- glarma(y, X, thetaLags = 7, type = "NegBin", method = "NR",
                                         residuals = "Pearson", alphaInit = 0,
                                         maxit = 100, grad = 1e-6)



covid.full.ds.train.3$Bitcoin.ret<-as.factor(covid.full.ds.train.3$Bitcoin.ret)
##Bitcoin
rf.covid.bitcoin<-randomForest(Bitcoin.ret~.,
                               data=covid.full.ds.train.3 %>% dplyr::select(Cases.diff:TSA,Bitcoin.ret)
                               ,type="classification")

yhat.rf.bitcoin<-predict(rf.covid.bitcoin, newdata = covid.full.ds.test.3)
mean((yhat.rf.bitcoin-covid.full.ds.test.3$Bitcoin.ret)^2)
importance(rf.covid.bitcoin)
varImpPlot(rf.covid.bitcoin)

data.frame(yhat.rf.bitcoin,covid.full.ds.test.3$Bitcoin.ret)
