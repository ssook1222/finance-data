library(dplyr)
please<-read.csv("C:\\data_fi\\전체총합.csv")
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

#stepwise 로 유의미한 결과만 남겨보기
step(lm.please2)


please3<-step(lm.please2)$coefficients
please4<-please3[27:50]
mybp<-barplot(please4,names=c("강동","강북","강서","관악","광진","구로","금천","노원","도봉","동대","동작","마포","서대","서초","성동","성북","송파","양천","영등","용산","은평","종로","중랑","중"))
text(x=mybp,y=please4,labels=please4,pos=3,col="black")