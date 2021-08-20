#라이브러리 설치
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("smooth")
library(smooth)

#데이터 불러오기
sj<-read.csv("./data/sangjone.csv",fileEncoding = "utf-8", quote="",row.names=NULL)
seoul_all<-sj$X.서울시.전체

View(sj)

#시계열 데이터로 변환, 2는 19년도 4월~6월 데이터
seoul_all_ts<-ts(seoul_all)
plot.ts(seoul_all_ts) 

#MA 모형 생성
seoul_all_ts_ma<-sma(seoul_all_ts,n=3) #3회의 관측치에 대한 이동평균 값

#원 데이터가 차분을 한 데이터였으므로 그대로 ARIMA 모형 확인
acf(seoul_all_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(seoul_all_ts,lag.max=20) # lag 절단값=0 -> AR(0)

#데이터 활용하여 최적의 ARIMA 모형 선택
auto.arima(seoul_all_ts)

# 선정된 ARIMA 모형으로 데이터 보정
seoul_all_arima<-arima(seoul_all_ts, order=c(1,0,0))
seoul_all_arima
# ARIMA 모형에 의해 보정된 데이터를 통해 미래값을 예측
predict(seoul_all_arima,n.ahead = 4) #2021년 2,3,4 + 22년 1분기 예측
