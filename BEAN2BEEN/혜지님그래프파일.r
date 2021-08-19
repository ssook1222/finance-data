#지역구별 이동인구와 가맹점매출입금 사이의 상관관계
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

#회귀분석하기
lm.please2<-lm(가맹점매출입금~지역구*이동인구, data=please)
summary(lm.please2)

#지역구가 범주형이므로 anova 실행
anova(lm.please2)

#stepwise 로 유의미한 결과만 남겨보기 (이게 어제 혜지님께 캡쳐해드린 콘솔창 결과와 동일)
step(lm.please2)
