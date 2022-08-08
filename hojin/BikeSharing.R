### 파일 불러오고 관측값 및 변수, 각 컬럼마다 NA 값 확인
df <- read.csv("./hojin/train.csv", header = T)
str(df)
for (i in 1:length(df)){
    cat(colnames(df)[i],sum(is.na(df[,i])),'\n')
}

## 데이터 요약
summary(df)



# 숫자형으로 표현하기 위해 날짜 데이터는
# 년, 월, 시간, 요일로 따로 데이터를 부르기
library(dplyr)
traindf <- traindf %>%
    select(-casual, -registered) %>%
    mutate(
        year = year(datetime),
        month = month(datetime),
        hour = hour(datetime),
        wday = wday(datetime))

traindf





# 각 변수별 Count와의 관계
barplot(table(df$datetime, df$season))

barplot(df$count, table(df$datetime))

barplot(df$count, df$holiday)


df$datetime
df$season

# barplot(df$datetime, xlab = datetime, ylab = season)





