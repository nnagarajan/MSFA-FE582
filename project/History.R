theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position=position_jitter(w=0.02, h=0.1)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position=position_jitter(w=0.02, h=0)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position=position_jitter(w=0, h=0)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position=position_jitter(w=0, h=0.1)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line() +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position = positoin_dodge(width=0.2)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position = position_dodge(width=0.2)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position = position_dodge(width=0.5)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
importance(rf.covid.spx)
varImpPlot(rf.covid.spx)
sqrt
sqrt(10)
importance(rf.covid.spx)
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
library(caret)
library(xgboost)
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
ggcorr(covid.full.ds %>% dplyr::select(Date:Cases,Deaths:Unemployment), method = c("everything", "pearson"))
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position = position_dodge(width=0.5)) +
scale_x_date(date_labels = "%b %y", date_breaks  = "2 month")+
theme(legend.position="bottom")
covid.full.ds<- covid.full.ds %>% dplyr::mutate(rnum = row_number())
covid.full.ds<-covid.full.ds %>% arrange(Date)
covid.full.ds %>% mutate(Cases=scale(Cases),Deaths=scale(Deaths),TSA=scale(TSA),GDP=scale(GDP),
Unemployment=scale(Unemployment),Oil=scale(Oil),Gold=scale(Gold),Bitcoin=scale(Bitcoin),
NDX=scale(NDX),DJIA=scale(DJIA),SPX=scale(SPX)) %>%
pivot_longer(c(Cases,Deaths,TSA,GDP,Unemployment,Oil,Gold,Bitcoin,NDX,DJIA,SPX), names_to = "variable",values_to ="values") %>%
dplyr::select(Date,variable,values) %>%
ggplot(mapping = aes(x=Date,y=values,color=variable)) +
geom_line(position = position_dodge(width=0.5)) +
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
#Split into test and train
covid.full.ds.train <- covid.full.ds %>% dplyr::filter(Date<as.Date("2021-02-01"), Date>as.Date("2019-01-02"))
covid.full.ds.test  <- anti_join(covid.full.ds  %>% dplyr::filter(Date>as.Date("2019-01-02")) , covid.full.ds.train, by = 'rnum')
#Filter required data
covid.full.ds.train.1<- covid.full.ds.train %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
covid.full.ds.test.1<- covid.full.ds.test %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
#Split into test and train
covid.full.ds.train <- covid.full.ds %>% dplyr::filter( Date>as.Date("2019-01-02"))  %>% sample_frac(.70)
covid.full.ds.test  <- anti_join(covid.full.ds  %>% dplyr::filter(Date>as.Date("2019-01-02")) , covid.full.ds.train, by = 'rnum')
covid.full.ds.train.2<- covid.full.ds.train %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
covid.full.ds.test.2<- covid.full.ds.test %>% dplyr::select(Date,Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend,NDX.trend,DJIA.trend,Bitcoin.trend)
covid.full.ds.train.1
covid.full.ds.test.1
set.seed(3)
rf.covid.spx<-randomForest(SPX.trend~.,data=covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP,SPX.trend),
mtry=3, tree=2000,importance=TRUE)
yhat.rf.spx<-predict(rf.covid.spx, newdata = covid.full.ds.test.2)
confusionMatrix(yhat.rf.spx,covid.full.ds.test.2$SPX.trend)
importance(rf.covid.spx)
varImpPlot(rf.covid.spx)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree")
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree", gamma = 0.1)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree", gamma = 0.01)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree", gamma = 0.3)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree", gamma = 5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="gbtree", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
#BTC
xgboost.covid.btc<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$Bitcoin.trend=="Up",1,0), max_depth = 1000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic", subsample=0.5 )
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
#BTC
xgboost.covid.btc<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$Bitcoin.trend=="Up",1,0), max_depth = 1000, eta = 0.1,
nrounds = 10000, objective = "binary:logistic", subsample=1 )
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="dart", gamma = 0.5, subsample=0.5)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5, subsample=0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5, subsample=01)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 100, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.2,"Up","Down")),covid.full.ds.test.2$SPX.trend)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
yhat.xgboost.btc.imp.raw<-xgb.importance(model = xgboost.covid.btc)
yhat.xgboost.btc<-predict(xgboost.covid.btc, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.btc>0.2,"Up","Down")),covid.full.ds.test.2$Bitcoin.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.2,"Up","Down")),covid.full.ds.test.2$SPX.trend)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
yhat.xgboost.spx.imp.raw<-xgb.importance(model = xgboost.covid.spx)
xgb.plot.importance(importance_matrix = yhat.xgboost.spx.imp.raw)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 10000, objective = "binary:logistic",booster="dart", gamma = 0.5)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 2000, objective = "binary:logistic",booster="dart", gamma = 0.5)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5, sampling_method= "gradient_based",
subsample=1, tree_method="hist")
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5,
sampling_method= "gradient_based",
subsample=1,
tree_method="hist",
grow_policy="lossguide")
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
xgboost.covid.spx<-xgboost(data = covid.full.ds.train.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix,
label = if_else(covid.full.ds.train.2$SPX.trend=="Up",1,0), max_depth = 10, eta = 0.1,
nrounds = 1000, objective = "binary:logistic",booster="dart", gamma = 0.5,
sampling_method= "gradient_based",
subsample=1,
tree_method="hist",
grow_policy="lossguide",
num_parallel_tree=2)
yhat.xgboost.spx<-predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
confusionMatrix(as.factor(if_else(yhat.xgboost.spx>0.5,"Up","Down")),covid.full.ds.test.2$SPX.trend)
yhat.xgboost.spx<-xgboost.Booster.predict(xgboost.covid.spx, newdata = covid.full.ds.test.2 %>% dplyr::select(Cases,Deaths,Oil,Gold,TSA,TLT,Unemployment,GDP) %>% as.matrix)
