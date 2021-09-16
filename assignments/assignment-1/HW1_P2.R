library("ggplot2")
library('gdata')
library('dplyr')
library('lubridate')
perl<-"C:\\Strawberry/perl/bin/perl.exe"
options(scipen=999)

setwd("Z:\\Desktop\\workspace\\stevens-fa\\FE582WS\\assignments\\assignment-1\\HW1_S21")
## load data set
nyt1<-read.csv("nyt1.csv")
nyt2<-read.csv("nyt2.csv")
nyt3<-read.csv("nyt3.csv")

nyt1$day<-"Day-1"
nyt2$day<-"Day-2"
nyt3$day<-"Day-3"

nytset<-dplyr::bind_rows(nyt1,nyt2,nyt3)

dssummary<-data.frame("Day"=c("Day1","Day2","Day3","Total"),
                      "Count"=c(count(nyt1)[[1]],
                                count(nyt2)[[1]],
                                count(nyt3)[[1]],
                                count(nytset)[[1]]))
knitr::kable(dssummary, col.names = gsub("[.]", " ", names(dssummary)))

names(nytset)<-tolower(names(nytset))
summary(nytset)
nytset$day<-as.factor(nytset$day)
nytset<- nytset %>% mutate(gender=case_when(gender=="0" ~ "Female", gender=="1"~"Male"))
nytset$gender<-as.factor(nytset$gender)
nytset$age_group<-cut(nytset$age, c(-Inf,19,29,39,49,59,69,+Inf))

age_cat_label<- c("<20", "20-29","30-39","40-49","50-59","60-69","70+")

# distribution age_group vs CTR
nytset %>% filter(impressions>0, age>0) %>% group_by(age_group) %>% 
  summarise(count_imps=sum(impressions),mean(impressions),median(impressions),IQR(impressions))

nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(mapping = aes(x=age_group,y=impressions)) + geom_boxplot() + 
  scale_x_discrete(labels=age_cat_label)+
  facet_wrap(~day,ncol=1)
nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(mapping = aes(x=impressions)) + geom_histogram() +
  facet_wrap(~day,ncol=1)
nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(aes(x=clicks,fill=age_group))+geom_density()+
  scale_x_discrete(labels=age_cat_label)+
  facet_wrap(~day,ncol=1) 

nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(mapping = aes(x=age_group,y=clicks/impressions)) + geom_boxplot() + 
  scale_x_discrete(labels=age_cat_label)+
  facet_wrap(~day,ncol=1)
nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(mapping = aes(x=clicks/impressions)) + geom_histogram() +
  facet_wrap(~day,ncol=1)

nytset %>% filter(impressions>0, age>0, clicks >0) %>% 
  ggplot(aes(x=clicks/impressions,fill=age_group))+geom_density()+
  facet_wrap(~day,ncol=1)  
  
nytset %>% filter(clicks>0) %>% group_by(day,age_group) %>% 
  summarise(CTR=(sum(clicks)/sum(impressions))*100, ttlclick=sum(clicks),ttlimps=sum(impressions)) %>%
  ggplot(mapping = aes(x=age_group,y=CTR)) + 
  geom_col( color="black", width=0.4,mapping=aes(fill=day), position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels=age_cat_label)+
  xlab(label = "Age Group")+
  facet_wrap(~day,ncol=1)

# Categorize based on Clicked, Not Clicked
nytset<- nytset %>% mutate(click_cat=case_when(clicks>0 ~ "Clicked", clicks<=0 ~ "Not Clicked"))
nytset$click_cat<-as.factor(nytset$click_cat)
summary(nytset$click_cat)

# Quantitative comparison across segments/demo
nytset %>% group_by(day,age_group, gender,click_cat) %>% 
  count(name="counts") %>%
  ggplot(mapping = aes(x=age_group,y=counts )) + geom_bar(mapping=aes(fill=click_cat),stat="identity", position = "dodge") + 
  facet_wrap(gender~day, ncol =3) + xlab("Age Group")

nytset %>% filter(clicks>0) %>% group_by(day,age_group, gender) %>% 
  summarise(CTR=(sum(clicks)/sum(impressions))*100, ttlclick=sum(clicks),ttlimps=sum(impressions)) %>%
  ggplot(mapping = aes(x=age_group,y=CTR)) + geom_bar(mapping=aes(fill=gender),stat="identity", position = "dodge") + 
  scale_x_discrete(labels=age_cat_label)+
  facet_wrap(~day, ncol =1)

nytset %>% filter(clicks>0) %>% group_by(day,gender) %>% 
  summarise(CTR=(sum(clicks)/sum(impressions))*100, ttlclick=sum(clicks),ttlimps=sum(impressions)) %>%
  ggplot(mapping = aes(x="",y=CTR)) + geom_bar(mapping=aes(fill=gender),stat="identity", position = "dodge")+
  facet_wrap(~day, ncol =3)+ylab("Click through rate") + xlab("") +
  scale_y_continuous(limit=c(0,30))

# Extend analysis across days
nytset %>% filter(clicks>0) %>% 
  group_by(day, gender) %>% 
  summarise(CTR=(sum(clicks)/sum(impressions))*100, ttlclick=sum(clicks),ttlimps=sum(impressions)) %>%
  #filter(gender=="Female") %>%
  ggplot(mapping = aes(x=day,y=CTR)) + geom_line(mapping=aes(group=gender,color=gender))


nytset %>% filter(clicks>0) %>% 
  group_by(day, age_group) %>% 
  summarise(CTR=(sum(clicks)/sum(impressions))*100, ttlclick=sum(clicks),ttlimps=sum(impressions)) %>%
  #filter(gender=="Female") %>%
  ggplot(mapping = aes(x=age_group,y=CTR)) + 
  geom_bar(mapping=aes(fill=day), stat="identity", position = "dodge") +
  scale_x_discrete(labels=age_cat_label)+
  scale_y_continuous(limit=c(0,50)) + xlab("Age Group") + ylab("Click through rate")


