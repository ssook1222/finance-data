install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages('tseries')
library(tseries)
install.packages("smooth")
library(smooth)

#데이터 불러오기
move<-read.csv("C:\\data_fi\\CARD_SUBWAY_MONTH.csv")
head(move)

move <- move %>%
  mutate(총계 = 승차총승객수+하차총승객수)

move <- move %>%  dplyr::select(사용일자, 역명, 총계)
head(move)
names(move)[1] <- c("기준년월")



#서울전체
agg_move <- aggregate(총계~기준년월, move, mean)
head(agg_move)



agg_move.ts <- ts(agg_move$총계)
plot(agg_move.ts, main="서울전체")

agg_move.ts_ma<-sma(agg_move.ts,n=3) #3회의 관측치에 대한 이동평균 값


agg_move.tsdiff1 <- diff(agg_move.ts, differences = 1)
agg_move.tsdiff2 <- diff(agg_move.ts, differences = 2)
agg_move.tsdiff3 <- diff(agg_move.ts, differences = 3)

plot.ts(agg_move.tsdiff1)
plot.ts(agg_move.tsdiff2)
plot.ts(agg_move.tsdiff3)
acf(agg_move.tsdiff3)
pacf(agg_move.tsdiff3)


auto.arima(agg_move$총계)
arima.agg_move.1.3.4 <- arima(agg_move$총계, order = c(1,3,4))
arima.agg_move.0.1.0 <- auto.arima(agg_move$총계)
arima.agg_move.1.3.4$aic  #더 작음
arima.agg_move.0.1.0$aic  


predict(arima.agg_move.1.3.4,n.ahead = 12)
plot(predict(arima.agg_move.1.3.4,n.ahead = 12)$pred)

