library(dplyr)
covidmov<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\코로나와 이동인구.csv")
covidmov

#수치형 자료임을 확인(지역구 제외 모두 수치형 자료)
sapply(covidmov, is.numeric)

#결측치 포함 관측개체는 제거하기
covidmov<-covidmov[complete.cases(covidmov),]
covidmov
table(is.na(covidmov))
covidmov

#상관관계 알아보기
cor.test(covidmov$이동인구, covidmov$확진자, method="pearson")

#이동인구와 확진자 회귀분석
library(ggplot2)
ggplot(data = covidmov, aes(x= 이동인구, y= 확진자)) + geom_point() +  coord_cartesian(xlim = c(0, 38000000), ylim = c(0, 600)) +stat_smooth(color = "#FC4E07", method = "lm")

summary(lm(확진자~이동인구, data=covidmov))
