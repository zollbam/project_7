## 혜진
df_hye<-read.csv('./hyejin/house-prices/train.csv',header=T)
df_hye$YrSold<-factor(df_hye$YrSold)
year_price<-df_hye%>%
    group_by(YrSold)%>%
    summarise(mean_price=mean(SalePrice))
mycolor=1:nrow(year_price)
barplot(mean_price~YrSold,data=year_price,col=mycolor)

## 수아
df_su<-read.csv('./sua/diabetes/diabetes.csv',header=T)
zero.na_df_su<-df_su[]
class(df_su)
library(VIM)
aggr(df_su,numbers=T,prop=F,sortVar=T)

# 1~8열까지 이상치 구하기
# Q1-1.5*IQR~Q3+1.5*IQR
df_su_want<-df_su[,1:8]

out_va<-c()
for (i in 1:ncol(df_su_want)){
    print(colnames(df_su_want)[i])
    q1<-quantile(df_su_want[,i])[2]
    q3<-quantile(df_su_want[,i])[4]
    irq<-q3-q1
    max_out<-q3+1.5*irq
    min_out<-q1-1.5*irq
    out_va_i<-c(min_out,max_out)
    out_va<-c(out_va,out_va_i)
}

## 영효
df_young<-read.csv('./younghyo/TopRamenRatings.csv',header=T,fileEncoding = "euc-kr")
df_young$Stars<-as.numeric(df_young$Stars)
View(df_young)
library(dplyr)
df_young%>%
    filter(df_young$Country=='South Korea')%>%
    summarise(mean=mean(df_young$Stars,na.rm=T))
sp_ta_rast<-sort(table(df_young$Style))
View(sp_ta_rast)
library(plotrix)
mycolor<-1:ncol(sp_ta_rast)
mycolor
fan.plot(sp_ta_rast,col=mycolor,labels=paste(names(sp_ta_rast),sp_ta_rast),radius=1)

## 호진
df_ho<-read.csv('./hojin/train.csv',header=T)
View(df_ho)
