### 파일 불러오고 관측값 및 변수, 각 컬럼마다 NA 값 확인
df <- read.csv("./hojin/train.csv", header = T)
str(df)
for (i in 1:length(df)){
    cat(colnames(df)[i],sum(is.na(df[,i])),'\n')
}

## 데이터 요약
summary(df)
str(df)
View(df)

# 숫자형으로 표현하기 위해 날짜 데이터는
# 년, 월, 시간, 요일로 따로 데이터를 부르기
# install.packages("dplyr")
library(dplyr)
# install.packages('lubridate')
library(lubridate)
traindf <- df %>%
    select(-casual, -registered) %>%
    mutate(
        year = year(datetime),
        month = month(datetime),
        hour = hour(datetime),
        wday = wday(datetime))


traindf$season <- factor(traindf$season, labels = c("Spring", "Summer", "Fall", "Winter"))
traindf$weather <- factor(traindf$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
traindf$holiday <- factor(traindf$holiday) # 0: 평일, 1:휴일
traindf$workingday <- factor(traindf$workingday) # 0: 휴일, 1:평일
traindf$year <- factor(traindf$year)
traindf$month <- factor(traindf$month)
traindf$hour <- factor(traindf$hour)
traindf$wday <- factor(traindf$wday, labels = c("Sun", "Mon", "Tue", "Wed", "Thu","Fir","Sat"))

str(traindf)
summary(traindf)

# ---------------------타입변경 완료------------------------------------

ye_ho_total<-traindf%>%
    group_by(year,hour)%>%
    summarise(to=sum(count))
View(ye_ho_total)
summary(ye_ho_total)
barplot(to~year+hour,data=ye_ho_total)


b<-ye_ho_total[ye_ho_total$year==2011,]
c<-ye_ho_total[ye_ho_total$year==2012,]

par(mfrow=c(1,2))
mycolor<-ifelse(b$to>60000,'blue','yellow')
barplot(to~hour,data=b, xlab='2011',col=mycolor)

mycolor<-ifelse(c$to>60000,'red','tomato')
barplot(to~hour,data=c, xlab='2012')
traindf$year<-factor(traindf$year)
# --------------------------------------------------------


se_we_count<-traindf%>%
    group_by(season,weather)%>%
    summarise(su=n())

se_we_count
barplot(su~weather+season,data=se_we_count)



cor(traindf$temp,traindf$windspeed)

str(traindf)

install.packages("corrplot")
library(corrplot)


traindf_nu <- traindf[, 6:9]
cor_matrix <- cor(traindf_nu)
cor_matrix
par(mfrow=c(1,1))
corrplot(cor_matrix)


df$season <- df$season, labels = c("Spring", "Summer", "Fall", "Winter")



# traindf[traindf$season=='Winter','count']
# sort(traindf[traindf$season=='Winter','count'],decreasing=T)
# sort(traindf[traindf$season=='Spring','count'],decreasing=T)
# sum(traindf[traindf$season=='Winter', 'count'])
# sum(traindf[traindf$season=='Spring', 'count'])






#-----------------------------------------
View(table(traindf$year,traindf$count))
barplot(table(traindf$year ~ (df$count)))
        
        
barplot(table(traindf$year,traindf$count))


barplot(table(traindf$wady,tarindf$count))


# 각 변수별 Count와의 관계
barplot(table(df$datetime, df$season))


df$datetime
df$season

# barplot(df$datetime, xlab = datetime, ylab = season)

traindf$season <- factor(traindf$season, labels = c("Spring", "Summer", "Fall", "Winter"))
traindf$weather <- factor(traindf$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
traindf$holiday <- factor(traindf$holiday)
traindf$workingday <- factor(traindf$workingday)
traindf$year <- factor(traindf$year)
traindf$month <- factor(traindf$month)
traindf$wday <- factor(traindf$wday, labels = c("Sun", "Mon", "Tue", "Wed", "Thu","Fir","Sat"))

View(traindf)

all_list <- (colnames(traindf) != "count")%>%
    which()

lst <- map(all_list, function(i)) {
    df_list <- colnames(traindf)[i]
    
    traindf %>%
        select(df_list, count) %>%
        rename(aa = df_list) %>%
        ggplot(aes(aa,count)) +
        geom_point(alpha=2, color = "#008ABC") +
        labs(title = paste0(df_list, " vs count"), x = df_list, y = "", color=df_list) +
        theme_bw() +
        theme(legend.position = "bottom")
    })

grid.arrange(grobs=lst, ncol=3)

