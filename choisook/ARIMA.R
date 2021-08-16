#라이브러리 설치
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)

#데이터 불러오기
corona<-read.csv("./corona_diff.csv", fileEncoding = "utf-8", quote="",row.names=NULL)

#데이터 변경
corona<-corona[,-c(1)]

#데이터 추출 및 결측치 제거 진행
jongro<-select(corona,X.종로구.dif.)
jongro<-na.omit(jongro)

#시계열 데이터로 변환, 15는 21년도 4월 데이터
jongro_ts<-ts(jongro)
plot.ts(jongro_ts)

#MA 모형 생성
jongro_ma<-SMA(jongro_ts,n=3) #3회의 관측치에 대한 이동평균 값
plot.ts(jongro_ma)

#원 데이터가 차분을 한 데이터였으므로 그대로 ARIMA 모형 확인
acf(jongro_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(0)
pacf(jongro_ts,lag.max=20) # lag 절단값=0 -> AR(0)

#데이터 활용하여 최적의 ARIMA 모형 선택
auto.arima(jongro_ts)

# 선정된 ARIMA 모형으로 데이터 보정
jongro_arima<-arima(jongro_ts, order=c(0,0,0))

# ARIMA 모형에 의해 보정된 데이터를 통해 미래값을 예측
jongro_fc<-forecast(jongro_arima,h=5)

