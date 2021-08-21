#라이브러리 설치
install.packages("dplyr")
library(dplyr)

#데이터 불러오기
reg<-read.csv("./data/regression.csv",fileEncoding = "utf-8", quote="",row.names=NULL)

#변수 재지정/지정
reg<-rename(reg,기준년월=X, 총소비금액=가격)

price<-reg$총소비금액
people<-reg$상존인구

m1<-lm(price~people)
plot(price~people,xlab="people",ylab="price")
abline(m1,col='red')
summary(m1)
