library("ggplot2")
library("xlsx")
library('gdata')
library('dplyr')
library('lubridate')
perl<-"C:\\Strawberry/perl/bin/perl.exe"
options(scipen=999)

setwd("Z:\\Desktop\\workspace\\stevens-fa\\FE582WS\\assignments\\assignment-1")

brooklyn<- read.xls("HW1_S21\\HW1\\rollingsales_brooklyn.xls", 1,pattern="BOROUGH",perl=perl)
bronx<- read.xls("rollingsales_bronx.xls", 1,pattern="BOROUGH",perl=perl)
manhattan<- read.xls("rollingsales_manhattan.xls", 1,pattern="BOROUGH",perl=perl)
queens<- read.xls("rollingsales_queens.xls", 1,pattern="BOROUGH",perl=perl)
statenisland<-read.xls("rollingsales_statenisland.xls",1,pattern="BOROUGH",perl=perl)


brooklyn$RESIDENTIAL.UNITS<-as.numeric(brooklyn$RESIDENTIAL.UNITS)
bronx$RESIDENTIAL.UNITS<-as.numeric(bronx$RESIDENTIAL.UNITS)
manhattan$RESIDENTIAL.UNITS<-as.numeric(manhattan$RESIDENTIAL.UNITS)
queens$RESIDENTIAL.UNITS<-as.numeric(queens$RESIDENTIAL.UNITS)
statenisland$RESIDENTIAL.UNITS<-as.numeric(statenisland$RESIDENTIAL.UNITS)

brooklyn$TOTAL.UNITS<-as.numeric(brooklyn$TOTAL.UNITS)
bronx$TOTAL.UNITS<-as.numeric(bronx$TOTAL.UNITS)
manhattan$TOTAL.UNITS<-as.numeric(manhattan$TOTAL.UNITS)
queens$TOTAL.UNITS<-as.numeric(queens$TOTAL.UNITS)
statenisland$TOTAL.UNITS<-as.numeric(statenisland$TOTAL.UNITS)

brooklyn$COMMERCIAL.UNITS<-as.numeric(brooklyn$COMMERCIAL.UNITS)
bronx$COMMERCIAL.UNITS<-as.numeric(bronx$COMMERCIAL.UNITS)
manhattan$COMMERCIAL.UNITS<-as.numeric(manhattan$COMMERCIAL.UNITS)
queens$COMMERCIAL.UNITS<-as.numeric(queens$COMMERCIAL.UNITS)
statenisland$COMMERCIAL.UNITS<-as.numeric(statenisland$COMMERCIAL.UNITS)

#merge data
bdset<-dplyr::bind_rows(bronx,brooklyn,manhattan,queens,statenisland)
names(bdset)<-tolower(names(bdset))
bdset$borough[bdset$borough=="1"]<-"manhattan"
bdset$borough[bdset$borough=="2"]<-"bronx"
bdset$borough[bdset$borough=="3"]<-"brooklyn"
bdset$borough[bdset$borough=="4"]<-"queens"
bdset$borough[bdset$borough=="5"]<-"statenisland"

summary(bdset)

#define types - numeric, date
bdset$land.square.feet <- as.numeric(gsub("[^[:digit:]]","",bdset$land.square.feet))
bdset$gross.square.feet <- as.numeric(bdset$gross.square.feet)
bdset$sale.price <- as.numeric(gsub("[^[:digit:]]","",bdset$sale.price))
bdset$sale.date <- as.Date(bdset$sale.date)
bdset$building.class.category<-trim(bdset$building.class.category)

summary(bdset)
head(bdset)

# missing values
unique(bdset$building.class.category)
bdset<-bdset %>% mutate(building.class.category=na_if(building.class.category,""))
unique(bdset$building.class.category)

bdset$building.class.category<- as.factor(bdset$building.class.category)
summary(bdset$building.class.category)
bdset$borough<- as.factor(bdset$borough)
summary(bdset)

# outlier

ggplot(bdset1,mapping = aes(x=log(sale.price)))+geom_histogram( binwidth = 1, fill="green", color="black")
ggplot(bdset1,mapping = aes(x=log(sale.price)))+geom_density(fill="blue")
ggplot(bdset1)+geom_boxplot(mapping = aes(y=log(sale.price)))
egg::ggarrange(ggplot(bdset1,mapping = aes(x=log(sale.price)))+geom_density(fill="blue"),
               ggplot(bdset1)+geom_boxplot(mapping = aes(x=log(sale.price))), heights=2:1)


bdset1<- bdset %>% mutate(sale.date.mw = floor_date(sale.date,'month'))



bdset1 <- bdset1 %>%
  mutate(
    buildingclasscat = case_when(
      building.class.category=="01  ONE FAMILY HOMES" ~ "ONE FAMILY HOME",
      building.class.category=="02  TWO FAMILY HOMES"        ~ "TWO FAMILY HOME",
      building.class.category=="03  THREE FAMILY HOMES"  ~  "THREE FAMILY HOME",
      grepl("COOPS",building.class.category) == TRUE ~ "COOPS",
      grepl("CONDOS",building.class.category) == TRUE ~ "CONDOS",
      TRUE ~ "NA"
    )
  ) 

bdset1$buildingclasscat <- as.factor(bdset1$buildingclasscat)



bdset2<- bdset1 %>% filter(log(sale.price)>5, log(sale.price)<=15,buildingclasscat %in% c("ONE FAMILY HOME","TWO FAMILY HOME","THREE FAMILY HOME","COOPS","CONDOS"))


#summary

bdset2 %>% group_by(borough,buildingclasscat,sale.date.mw) %>% summarize(sale.price=mean(sale.price))

ggplot(bdset2%>% filter(borough=="bronx",buildingclasscat=="CONDOS"),aes(x=sale.date.mw,y=log(sale.price)) ) + geom_line()

ggplot(bdset2)+geom_bar(mapping=aes(x=buildingclasscat,fill=borough), position = "dodge")

ggplot(bdset2)+geom_bar(mapping=aes(x=buildingclasscat,fill=buildingclasscat)) + facet_wrap(~borough, ncol=2)

ggplot(bdset2)+
    stat_summary(aes(x=sale.date.mw,y=log(sale.price),group=borough,color=borough),fun=mean,geom="line") +  
    scale_x_date(NULL,date_breaks = "3 month",date_labels = "%b%y") + 
    facet_wrap(~buildingclasscat,ncol=2)

