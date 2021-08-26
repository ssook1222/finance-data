#라이브러리 설치
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#데이터 불러오기
mv<-read.csv("./data/mov.csv",fileEncoding = "utf-8", quote="",row.names=NULL)

# 시각화
plot(mv$y2019,col="blue")
plot(mv$y2021,col="red")

#시각화 각자
ggplot(data=mv,aes(x=time,y=y2021))+geom_point(colour="blue")
ggplot(data=mv,aes(x=time,y=y2019))+geom_point(colour="red")

