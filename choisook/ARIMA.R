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

#25개구 모형 만들기
#종로구 
jongro<-sj$X.종로구
jongro_ts<-ts(jongro)
acf(jongro_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(jongro_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(jongro_ts)
jongro_arima<-arima(jongro_ts, order=c(1,1,0))
jongro_arima
predict(jongro_arima,n.ahead = 4)

#중구
joong<-sj$X.중구
joong_ts<-ts(joong)
acf(joong_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(joong_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(joong_ts)
joong_arima<-arima(joong_ts, order=c(1,1,0))
joong_arima
predict(joong_arima,n.ahead = 4)

#용산구
ys<-sj$X.용산구
ys_ts<-ts(ys)
acf(ys_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(ys_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(ys_ts)
ys_arima<-arima(ys_ts, order=c(1,0,0))
ys_arima
predict(ys_arima,n.ahead = 4)

sd<-sj$X.성동구
sd_ts<-ts(sd)
acf(sd_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(sd_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(sd_ts)
sd_arima<-arima(sd_ts, order=c(1,0,0))
sd_arima
predict(sd_arima,n.ahead = 4)

kj<-sj$X.광진구
kj_ts<-ts(kj)
acf(kj_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(kj_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(kj_ts)
kj_arima<-arima(kj_ts, order=c(1,0,0))
kj_arima
predict(kj_arima,n.ahead = 4)

dd<-sj$X.동대문구
dd_ts<-ts(dd)
acf(dd_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(dd_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(dd_ts)
dd_arima<-arima(dd_ts, order=c(1,0,0))
dd_arima
predict(dd_arima,n.ahead = 4)

jr<-sj$X.중랑구
jr_ts<-ts(jr)
acf(jr_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(jr_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(jr_ts)
jr_arima<-arima(jr_ts, order=c(1,0,0))
jr_arima
predict(jr_arima,n.ahead = 4)

kb<-sj$X.강북구
kb_ts<-ts(kb)
acf(kb_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(kb_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(kb_ts)
kb_arima<-arima(kb_ts, order=c(1,0,0))
kb_arima
predict(kb_arima,n.ahead = 4)

db2<-sj$X.도봉구
db2_ts<-ts(db2)
acf(db2_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(db2_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(db2_ts)
db2_arima<-arima(db2_ts, order=c(1,0,0))
db2_arima
predict(db2_arima,n.ahead = 4)


nw<-sj$X.노원구
nw_ts<-ts(nw)
acf(nw_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(nw_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(nw_ts)
nw_arima<-arima(nw_ts, order=c(1,0,0))
nw_arima
predict(nw_arima,n.ahead = 4)

yp<-sj$X.은평구
yp_ts<-ts(yp)
acf(yp_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(yp_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(yp_ts)
yp_arima<-arima(yp_ts, order=c(1,0,0))
yp_arima
predict(yp_arima,n.ahead = 4)

sd2<-sj$X.서대문구
sd2_ts<-ts(sd2)
acf(sd2_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(sd2_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(sd2_ts)
sd2_arima<-arima(sd2_ts, order=c(1,0,0))
sd2_arima
predict(sd2_arima,n.ahead = 4)

mp<-sj$X.마포구
mp_ts<-ts(mp)
acf(mp_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(mp_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(mp_ts)
mp_arima<-arima(mp_ts, order=c(1,0,0))
mp_arima
predict(mp_arima,n.ahead = 4)

yc<-sj$X.양천구
yc_ts<-ts(yc)
acf(yc_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(yc_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(yc_ts)
yc_arima<-arima(yc_ts, order=c(1,0,0))
yc_arima
predict(yc_arima,n.ahead = 4)

gs<-sj$X.강서구
gs_ts<-ts(gs)
acf(gs_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(gs_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(gs_ts)
gs_arima<-arima(gs_ts, order=c(1,0,0))
gs_arima
predict(gs_arima,n.ahead = 4)

gr<-sj$X.구로구
gr_ts<-ts(gr)
acf(gr_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(gr_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(gr_ts)
gr_arima<-arima(gr_ts, order=c(1,0,0))
gr_arima
predict(gr_arima,n.ahead = 4)

gc<-sj$X.금천구
gc_ts<-ts(gc)
acf(gc_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(gc_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(gc_ts)
gc_arima<-arima(gc_ts, order=c(1,0,0))
gc_arima
predict(gc_arima,n.ahead = 4)

yd<-sj$X.영등포구
yd_ts<-ts(yd)
acf(yd_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(yd_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(yd_ts)
yd_arima<-arima(yd_ts, order=c(1,0,0))
yd_arima
predict(yd_arima,n.ahead = 4)

dj<-sj$X.동작구
dj_ts<-ts(dj)
acf(dj_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(dj_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(dj_ts)
dj_arima<-arima(dj_ts, order=c(1,0,0))
dj_arima
predict(dj_arima,n.ahead = 4)

kw<-sj$X.관악구
kw_ts<-ts(kw)
acf(kw_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(kw_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(kw_ts)
kw_arima<-arima(kw_ts, order=c(1,0,0))
kw_arima
predict(kw_arima,n.ahead = 4)

sc<-sj$X.서초구
sc_ts<-ts(sc)
acf(sc_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(sc_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(sc_ts)
sc_arima<-arima(sc_ts, order=c(1,1,0))
sc_arima
predict(sc_arima,n.ahead = 4)

gn<-sj$X.강남구
gn_ts<-ts(gn)
acf(gn_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(gn_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(gn_ts)
gn_arima<-arima(gn_ts, order=c(1,0,0))
gn_arima
predict(gn_arima,n.ahead = 4)

sp<-sj$X.송파구
sp_ts<-ts(sp)
acf(sp_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(sp_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(sp_ts)
sp_arima<-arima(sp_ts, order=c(1,0,0))
sp_arima
predict(sp_arima,n.ahead = 4)

gd<-sj$X.강동구
gd_ts<-ts(gd)
acf(gd_ts,lag.max=20) # lag 1부터 점선 안에 존재. lag 절단값=1 -> MA(1)
pacf(gd_ts,lag.max=20) # lag 절단값=0 -> AR(0)
auto.arima(gd_ts)
gd_arima<-arima(gd_ts, order=c(1,0,0))
gd_arima
predict(gd_arima,n.ahead = 4)
