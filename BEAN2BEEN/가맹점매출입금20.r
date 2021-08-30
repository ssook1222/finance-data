library(dplyr)
covid20<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\2020부터전체총합.csv")
covid20

#수치형 자료임을 확인(지역구 제외 모두 수치형 자료)
sapply(covid20, is.numeric)



#결측치 포함 관측개체는 제거하기
covid20<-covid20[complete.cases(covid20),]
covid20
table(is.na(covid20))

covid20

#prediction 모델 회귀분석으로 만들기
library(DMwR2)
lm20<-lm(covid20$가맹점매출입금~. , data=covid20[,2:9])
summary(lm20)

#공변량에 지역구라는 범주형이 포함되어있으므로 anova 사용하기
anova(lm20)

#알아서 필요한 변수를 선택하도록 step sequence 적용하기
step(lm20)

#Regression Tree 만들어보기
library(rpart)
rt20<-rpart(covid20$가맹점매출입금~., data=covid20)
rt20

#Tree 시각화를 위해 rpart.plot 패키지 설치
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rt20, digits=2, fallen.leaves=T, type=4, cex=0.7)

#predict 함수를 통해 테스트 데이터셋 평가 진행
p.rt20<-predict(rt20, covid20)
summary(p.rt20)

summary(covid20$가맹점매출입금)

#예측값과 트레이닝 데이터셋 상관관계
cor(p.rt20, covid20$가맹점매출입금)

#평균절대오차값 만드는 함수로 오차에 대한 절대값의 평균이 어느정도인지 알아보기
MAE<-function(actual, predicted){
  mean(abs(actual-predicted))
}

MAE(p.rt20, covid20$가맹점매출입금)


mean(covid20$가맹점매출입금)

#평균절대오차 구하기
MAE(19941224, covid20$가맹점매출입금)

library(rattle)
fancyRpartPlot(rt20,cex=0.7)

#randomForest 패키지 이용해서 예측 모델 생성하기
install.packages("randomForest")
library(randomForest)

co.randomF<-randomForest(covid20$가맹점매출입금~., data=covid20, ntree= 400, proximity=TRUE)
plot(co.randomF)

#예측 모델 특성 살피기
importance(co.randomF)
p.randomF<-predict(co.randomF, newdata=covid20)
summary(covid20$가맹점매출입금)
cor(p.randomF, covid20$가맹점매출입금)
MAE(p.randomF, covid20$가맹점매출입금)

