library(tidyverse)
train <- read_csv("./hyejin/house-prices/train.csv")
# Let's take a look at the data
head(train)
# Let's discover the statistics of the data
summary(train)
# It looks like there are some NA values:
sum(is.na(train))
sum(!complete.cases(train)) # NA 있는 행의 수 

# 종속변수 describe
library(psych)
describe(train$SalePrice)

# 컬럼별 결측치 합 
colSums(sapply(train, is.na))

library("VIM")
library("ggplot2")
aggr(train$Fence,prop=FALSE,numbers=TRUE)

summary(train$Fence) # class character, length 14610
sum(is.na(train$Fence)) # NA 1179개
sum(complete.cases(train$Fence)) # NA 아닌 281개   

aggr(train,prop=FALSE,numbers=TRUE)


## GARAGE

class(train$GarageType)

table(train$GarageType)

train[train$GarageType=='NA',"GarageType"]
train[train$GarageType=='NA', ]
train[ , c(59,61,64,65)] # type, finish, quality, condition
train[ , 60] # year

#train[is.na(train$GarageType),'GarageType'] <-'NG'
colnames(train)[c(59,61,64,65)]
train[is.na(train$GarageType), colnames(train)[c(59,61,64,65)]] <-'NG'

summary(train$Fence)

# 컬럼별 결측치 합 
colSums(sapply(train, is.na))

# -------------------------------------
options(scipen=999) # 지수 표기 바꾸기 
plot(SalePrice ~ LotArea, data=train, col= 2:4)
plot(SalePrice ~ GrLivArea, data=train, col= 1:3)

#값 단위 차이가 너무 큼 
boxplot(train$SalePrice, train$LotArea, col='blue')  
boxplot(train$SalePrice, train$GrLivArea, col ='red')

# saleprice 히스토그램 
hist(train$SalePrice,col = 'lightblue', border = 'black',
         main = 'SalePrice 히스토그램', xlab = 'SalePrice', ylab='count')

# 1.정규분포에서 살짝 벗어난다. 
# 2.데이터의 분포가 한쪽으로 쏠린 왜도 현상이 보인다.

plot(train$GrLivArea ,train$SalePrice, col = 'orange')
# X값이 증가함에 따라 Y값이 비례하여 증가하는, 
#기울기가 변화하지 않는 1차 함수관계이기 때문에 
# 이럴 경우 X 변수와 Y 변수는 선형 관계에 있다고 하며, 
#그래프를 선형그래프라고 합니다.

plot(train$TotalBsmtSF ,train$SalePrice, col = 'royalblue')
# totalbsmtSF도 선형관계에 있으며, 자기 자신이나 0에 가까운 분포도 보이고 있다.

#Relationship with categorical features
plot(train$OverallQual, train$SalePrice, ylim=c(0,800000))

# 상관계수 - NA yes
(train)[sapply(train, class) == 'numeric']
train_cor <- cor((train)[sapply(train, class) == 'numeric'])
train_cor
round(train_cor[, 'SalePrice'],1) # SP 기준으로 상관계수
train_cor[, 'SalePrice']
scale(train_cor) # 데이터 표준화 

library("corrplot")
plot(train[2:5])
corrplot(train_cor)

# ------------------------
# 상관계수가 높은 컬럼과 비교

# 집값과 전체 품질

plot(train$OverallQual, train$SalePrice, main='집값과 전체 품질',
     xlab='전체 품질(등급)', ylab='집값', col='orange', pch=15,
     xlim=c(1,10))

'''
컬럼 *OverallQual*은 집의 전반적인 재료와 마감을 평가합니다.
집의 전체 품질이 올라갈수록 집값이 올라가는 모습을 보입니다.
특이한게 최고 등급인 10등급에서 다양한 가격의 분포를 볼 수 있음.
'''

plot(train$GrLivArea, train$SalePrice, main='집값과 거실 면적',
     xlab='거실 면적(평방 피트)', ylab='집값', col='blue', pch=15,
     xlim=c())

outlier <- boxplot.stats(train$GrLivArea)$out
outlier
train[train$GrLivArea %in% outlier, ] <- NA
train.no.outlier <- na.omit(train)
nrow(train.no.outlier)

'''

'''

plot(factor(train$FullBath), train$SalePrice, main='집값과 전체 품질',
     xlab='욕실 수(0~3)', ylab='집값', col='green', pch=15)

library(dplyr)
MB <- train%>%
    group_by(FullBath)%>%
    summarise(meanBath=mean(SalePrice))
str(MB)
MB$FullBath<-as.factor(MB$FullBath)
barplot(MB$meanBath, names.arg = MB$FullBath)

# numeric 인 컬럼명만  
colnames(train)[sapply(train, class) == 'numeric'] 

summary(train[ , .SD, .SDcols = numeric_var])

# 함수 

# 캐릭터 네임
names(train)[which(sapply(train, is.character))]
cat_var <- names(train)[which(sapply(train, is.character))]

c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')

# 뉴머릭 네임 
names(train)[which(sapply(train, is.numeric))]
numeric_var <- names(train)[which(sapply(train, is.numeric))]



install.packages('data.table')
library(data.table)
library(dplyr)


head(train)
colSums(sapply(train, is.na))
colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))









