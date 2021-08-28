library(dplyr)
covid<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\전체총합.csv")
head(covid)


#결측치 포함 관측개체는 제거하기
covid<-covid[complete.cases(covid),]
covid
table(is.na(covid))

#이동인구와 가맹점매출입금 상관관계 구하기
cor.test(covid$이동인구, covid$가맹점매출입금, method="pearson")

#이동인구와 가맹점매출입금 회귀분석 진행
library(ggplot2)
ggplot(data = covid, aes(x= 이동인구, y= 가맹점매출입금)) + geom_point() + stat_smooth(se=F, color = "#FC4E07", method = "lm")

summary(lm(가맹점매출입금~이동인구, data=covid))
