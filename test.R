## 혜진
df_hye<-read.csv('./hyejin/',header=T)

## 수아
df_su<-read.csv('./sua/diabetes/diabetes.csv',header=T)
View(df_su)
sum(df_su[df_su$Insulin>=300,'Outcome'])/nrow(df_su[df_su$Insulin>=300,])
hist(df_su$Insulin,breaks=10)

## 영효
df_young<-read.csv('./younghyo/TopRamenRatings.csv',header=T,fileEncoding = "euc-kr")
df_young$Stars<-as.numeric(df_young$Stars)
View(df_young)
library(dplyr)
df_young%>%
    filter(df_young$Country=='South Korea')%>%
    summarise(mean=mean(df_young$Stars,na.rm=T))

## 호진
df_ho<-read.csv('./hojin/train.csv',header=T)
View(df_ho)















