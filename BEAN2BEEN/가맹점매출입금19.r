library(dplyr)
covid19<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\2019부터전체총합.csv")
covid19

#수치형 자료임을 확인(지역구 제외 모두 수치형 자료)
sapply(covid19, is.numeric)



#결측치 포함 관측개체는 제거하기
covid19<-covid19[complete.cases(covid19),]
covid19
table(is.na(covid19))

covid19

#prediction 모델 회귀분석으로 만들기
library(DMwR2)
lm19<-lm(covid19$가맹점매출입금~. , data=covid19[,2:9])
summary(lm19)

#공변량에 지역구라는 범주형이 포함되어있으므로 anova 사용하기
anova(lm19)

#알아서 필요한 변수를 선택하도록 step sequence 적용하기
step(lm19)

#Regression Tree 만들어보기
library(rpart)
rt19<-rpart(covid19$가맹점매출입금~., data=covid19)
rt19

#Tree 시각화를 위해 rpart.plot 패키지 설치
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rt19, digits=2, fallen.leaves=T, type=4, cex=0.7)

#predict 함수를 통해 테스트 데이터셋 평가 진행
p.rt19<-predict(rt19, covid19)
summary(p.rt19)

summary(covid19$가맹점매출입금)

#예측값과 트레이닝 데이터셋 상관관계
cor(p.rt19, covid19$가맹점매출입금)

#평균절대오차값 만드는 함수로 오차에 대한 절대값의 평균이 어느정도인지 알아보기
MAE<-function(actual, predicted){
  mean(abs(actual-predicted))
}

MAE(p.rt19, covid19$가맹점매출입금)


mean(covid19$가맹점매출입금)

#평균절대오차 구하기
MAE(21740239, covid19$가맹점매출입금)
