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
#anova(lm20)

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

#시각화를 더 예쁘게 하기 위해 rattle 패키지 설치
library(rattle)
fancyRpartPlot(rt20,cex=0.7)
