
library(dplyr)
please<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\전체총합.csv")
head(please)


#결측치 포함 관측개체는 제거하기
please<-please[complete.cases(please),]
please
table(is.na(please))

#총수신금액+총소비금액 합하기
please<- please %>% mutate(총계=총소비금액+총수신금액)
head(please)
attach(please)

#전체총합의 변수들은 지역구, 이동인구, 누적이동인구, 가맹점매출입금, 총소비금액, 총수신금액, 예적금금액 이 있음. 


#1. 이동인구와 가맹점매출입금 회귀분석 후 그래프 
library(ggplot2)
ggplot(data = please, aes(x= 이동인구, y= 가맹점매출입금))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(가맹점매출입금~이동인구, data=please))

#2. 총소비금액과 가맹점매출입금 회귀분석 후 그래프
library(ggplot2)
ggplot(data = please, aes(x= 총소비금액, y= 가맹점매출입금))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(가맹점매출입금~총소비금액, data=please))

#3. 총수신금액과 가맹점매출입금 회귀분석 후 그래프
library(ggplot2)
ggplot(data = please, aes(x= 총수신금액, y= 가맹점매출입금))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(가맹점매출입금~총수신금액, data=please))

#4. 총계(총수신금액+총소비금액)과 가맹점매출입금 회귀분석 후 그래프
library(ggplot2)
ggplot(data = please, aes(x= 총계, y= 가맹점매출입금))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(가맹점매출입금~총계, data=please))


#5. 총소비금액과 총수신금액 회귀분석 후 그래프
library(ggplot2)
ggplot(data = please, aes(x= 총소비금액, y= 총수신금액))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(총수신금액~총소비금액, data=please))


#6. 총소비금액과 이동인구 회귀분석 후 그래프
library(ggplot2)
ggplot(data = please, aes(x= 이동인구 , y= 총소비금액))+geom_point() + stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(총소비금액~이동인구, data=please))


