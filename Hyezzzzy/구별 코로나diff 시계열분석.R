install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages('tseries')
library(tseries)
install.packages("smooth")
library(smooth)

#데이터 불러오기
co<-read.csv("C:\\data_fi\\코로나 파일.csv")
head(co)

#구별 나누기
gangseo<-co[['강서구.dif']]
yang<-co[['양천구.dif']]
keum<-co[['금천구.dif']]
kwan<-co[['관악구.dif']]
eun<-co[['은평구.dif']]
guro<-co[['구로구.dif']]
dobong<-co[['도봉구.dif']]
jlang<-co[['중랑구.dif']]
gangdong<-co[['강동구.dif']]
yong<-co[['용산구.dif']]
kwang<-co[['광진구.dif']]
moon<-co[['동대문구.dif']]
noone<-co[['노원구.dif']]
kbook<-co[['강북구.dif']]
seo<-co[['서대문구.dif']]
seong<-co[['성북구.dif']]
jong<-co[['종로구.dif']]
jung<-co[['중구.dif']]
mapo<-co[['마포구.dif']]
yung<-co[['영등포구.dif']]
song<-co[['송파구.dif']]
gnam<-co[['강남구.dif']]
scho<-co[['서초구.dif']]
sdong<-co[['성동구.dif']]
djak<-co[['동작구.dif']]
gita<-co[['기타.dif']]



#강남구
gnam.ts <- ts(gnam)
plot(gnam.ts, main="강남구")

gnam.ts_ma<-sma(gnam.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(gnam.ts)
pacf(gnam.ts)


auto.arima(gnam)
arima.gnam.0.0.1 <- arima(gnam, order = c(0,0,1))
arima.gnam.0.1.0 <- auto.arima(gnam)
arima.gnam.0.0.1$aic 
arima.gnam.0.1.0$aic #더 작음

arima.gnam.0.1.0
predict(arima.gnam.0.1.0,n.ahead = 5)


#강동구
gangdong.ts <- ts(gangdong)
plot(gangdong.ts, main="강동구")

gangdong.ts_ma<-sma(gangdong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(gangdong.ts)
pacf(gangdong.ts)


auto.arima(gangdong)
arima.gangdong.0.0.1 <- arima(gangdong, order = c(0,0,1))
arima.gangdong.0.1.0 <- auto.arima(gangdong)
arima.gangdong.0.0.1$aic #더 작음
arima.gangdong.0.1.0$aic 

arima.gangdong.0.0.1
predict(arima.gangdong.0.0.1,n.ahead = 5)


#강북구
kbook.ts <- ts(kbook)
plot(kbook.ts, main="강북구")

kbook.ts_ma<-sma(kbook.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(kbook.ts)
pacf(kbook.ts)


auto.arima(kbook)
arima.kbook.0.0.1 <- arima(kbook, order = c(0,0,1))
arima.kbook.0.0.0 <- auto.arima(kbook)
arima.kbook.0.0.1$aic #더 작음
arima.kbook.0.0.0$aic 

arima.kbook.0.0.1
predict(arima.kbook.0.0.1,n.ahead = 5)



#강서구
gangseo.ts <- ts(gangseo)
plot(gangseo.ts, main="강북구")

gangseo.ts_ma<-sma(gangseo.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(gangseo.ts)
pacf(gangseo.ts)


auto.arima(gangseo)
arima.gangseo.0.0.1 <- arima(gangseo, order = c(0,0,1))
arima.gangseo.0.0.0 <- auto.arima(gangseo)
arima.gangseo.0.0.1$aic #더 작음
arima.gangseo.0.0.0$aic 

arima.gangseo.0.0.1
predict(arima.gangseo.0.0.1,n.ahead = 5)


#관악구
kwan.ts <- ts(kwan)
plot(kwan.ts, main="관악구")

kwan.ts_ma<-sma(kwan.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(kwan.ts)
pacf(kwan.ts)


auto.arima(kwan)
arima.kwan.0.0.1 <- arima(kwan, order = c(0,0,1))
arima.kwan.0.0.0 <- auto.arima(kwan)
arima.kwan.0.0.1$aic #더 작음
arima.kwan.0.0.0$aic 

arima.kwan.0.0.1
predict(arima.kwan.0.0.1,n.ahead = 5)

#광진구
kwang.ts <- ts(kwang)
plot(kwang.ts, main="광진구")

kwang.ts_ma<-sma(kwang.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(kwang.ts)
pacf(kwang.ts)


auto.arima(kwang)
arima.kwang.0.0.1 <- arima(kwang, order = c(0,0,1))
arima.kwang.0.0.0 <- auto.arima(kwang)
arima.kwang.0.0.1$aic #더 작음
arima.kwang.0.0.0$aic 

arima.kwang.0.0.1
predict(arima.kwang.0.0.1,n.ahead = 5)


#구로구
guro.ts <- ts(guro)
plot(guro.ts, main="구로구")

guro.ts_ma<-sma(guro.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(guro.ts)
pacf(guro.ts)


auto.arima(guro)
arima.guro.0.0.1 <- arima(guro, order = c(0,0,1))
arima.guro.0.0.0 <- auto.arima(guro)
arima.guro.0.0.1$aic #더 작음
arima.guro.0.0.0$aic 

arima.guro.0.0.1
predict(arima.guro.0.0.1,n.ahead = 5)



#금천구
keum.ts <- ts(keum)
plot(keum.ts, main="금천구")

keum.ts_ma<-sma(keum.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(keum.ts)
pacf(keum.ts)


auto.arima(keum)
arima.keum.0.0.1 <- arima(keum, order = c(0,0,1))
arima.keum.0.0.0 <- auto.arima(keum)
arima.keum.0.0.1$aic #더 작음
arima.keum.0.0.0$aic 

arima.keum.0.0.1
predict(arima.keum.0.0.1,n.ahead = 5)



#노원구
noone.ts <- ts(noone)
plot(noone.ts, main="노원구")

noone.ts_ma<-sma(noone.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(noone.ts)
pacf(noone.ts)


auto.arima(noone)
arima.noone.0.0.1 <- arima(noone, order = c(0,0,1))
arima.noone.0.0.0 <- auto.arima(noone)
arima.noone.0.0.1$aic #더 작음
arima.noone.0.0.0$aic 

arima.noone.0.0.1
predict(arima.noone.0.0.1,n.ahead = 5)




#도봉구
dobong.ts <- ts(dobong)
plot(dobong.ts, main="도봉구")

dobong.ts_ma<-sma(dobong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(dobong.ts)
pacf(dobong.ts)


auto.arima(dobong)
arima.dobong.0.0.1 <- arima(dobong, order = c(0,0,1))
arima.dobong.0.0.0 <- auto.arima(dobong)
arima.dobong.0.0.1$aic #더 작음
arima.dobong.0.0.0$aic 

arima.dobong.0.0.1
predict(arima.dobong.0.0.1,n.ahead = 5)

#동대문구
moon.ts <- ts(moon)
plot(moon.ts, main="동대문구")

moon.ts_ma<-sma(moon.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(moon.ts)
pacf(moon.ts)


auto.arima(moon)
arima.moon.0.0.1 <- arima(moon, order = c(0,0,1))
arima.moon.0.0.0 <- auto.arima(moon)
arima.moon.0.0.1$aic #더 작음
arima.moon.0.0.0$aic 

arima.moon.0.0.1
predict(arima.moon.0.0.1,n.ahead = 5)


#동작구
djak.ts <- ts(djak)
plot(djak.ts, main="동작구")

djak.ts_ma<-sma(djak.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(djak.ts)
pacf(djak.ts)


auto.arima(djak)
arima.djak.0.0.1 <- arima(djak, order = c(0,0,1))
arima.djak.0.0.0 <- auto.arima(djak)
arima.djak.0.0.1$aic #더 작음
arima.djak.0.0.0$aic 

arima.djak.0.0.1
predict(arima.djak.0.0.1,n.ahead = 5)



#마포구
mapo.ts <- ts(mapo)
plot(mapo.ts, main="마포구")

mapo.ts_ma<-sma(mapo.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(mapo.ts)
pacf(mapo.ts)


auto.arima(mapo)
arima.mapo.0.0.1 <- arima(mapo, order = c(0,0,1))
arima.mapo.0.0.0 <- auto.arima(mapo)
arima.mapo.0.0.1$aic
arima.mapo.0.0.0$aic  #더 작음

arima.mapo.0.0.0
predict(arima.mapo.0.0.0,n.ahead = 5)



#서대문구
seo.ts <- ts(seo)
plot(seo.ts, main="서대문구")

seo.ts_ma<-sma(seo.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(seo.ts)
pacf(seo.ts)


auto.arima(seo)
arima.seo.0.0.1 <- arima(seo, order = c(0,0,1))
arima.seo.0.0.0 <- auto.arima(seo)
arima.seo.0.0.1$aic
arima.seo.0.0.0$aic  #더 작음

arima.seo.0.0.0
predict(arima.seo.0.0.0,n.ahead = 5)




#서초구
scho.ts <- ts(scho)
plot(scho.ts, main="서초구")

scho.ts_ma<-sma(scho.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(scho.ts)
pacf(scho.ts)


auto.arima(scho)
arima.scho.0.0.1 <- arima(scho, order = c(0,0,1))
arima.scho.0.0.0 <- auto.arima(scho)
arima.scho.0.0.1$aic
arima.scho.0.0.0$aic  #더 작음

arima.scho.0.0.0
predict(arima.scho.0.0.0,n.ahead = 5)



#성동구
sdong.ts <- ts(sdong)
plot(sdong.ts, main="성동구")

sdong.ts_ma<-sma(sdong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(sdong.ts)
pacf(sdong.ts)


auto.arima(sdong)
arima.sdong.0.0.1 <- arima(sdong, order = c(0,0,1))
arima.sdong.0.0.0 <- auto.arima(sdong)
arima.sdong.0.0.1$aic
arima.sdong.0.0.0$aic  #더 작음

arima.sdong.0.0.0
predict(arima.sdong.0.0.0,n.ahead = 5)






#성북구
seong.ts <- ts(seong)
plot(seong.ts, main="성북구")

seong.ts_ma<-sma(seong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(seong.ts)
pacf(seong.ts)


auto.arima(seong)
arima.seong.0.0.1 <- arima(seong, order = c(0,0,1))
arima.seong.0.0.0 <- auto.arima(seong)
arima.seong.0.0.1$aic  #더 작음
arima.seong.0.0.0$aic 

arima.seong.0.0.1
predict(arima.seong.0.0.1,n.ahead = 5)




#송파구
song.ts <- ts(song)
plot(song.ts, main="송파구")

song.ts_ma<-sma(song.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(song.ts)
pacf(song.ts)


auto.arima(song)
arima.song.0.0.1 <- arima(song, order = c(0,0,1))
arima.song.0.0.0 <- auto.arima(song)
arima.song.0.0.1$aic  #더 작음
arima.song.0.0.0$aic 

arima.song.0.0.1
predict(arima.song.0.0.1,n.ahead = 5)




#양천구
yang.ts <- ts(yang)
plot(yang.ts, main="양천구")

yang.ts_ma<-sma(yang.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(yang.ts)
pacf(yang.ts)


auto.arima(yang)
arima.yang.0.0.1 <- arima(yang, order = c(0,0,1))
arima.yang.0.0.0 <- auto.arima(yang)
arima.yang.0.0.1$aic  #더 작음
arima.yang.0.0.0$aic 

arima.yang.0.0.1
predict(arima.yang.0.0.1,n.ahead = 5)




#영등포구
yung.ts <- ts(yung)
plot(yung.ts, main="영등포구")

yung.ts_ma<-sma(yung.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(yung.ts)
pacf(yung.ts)


auto.arima(yung)
arima.yung.0.0.1 <- arima(yung, order = c(0,0,1))
arima.yung.0.0.0 <- auto.arima(yung)
arima.yung.0.0.1$aic  #더 작음
arima.yung.0.0.0$aic 

arima.yung.0.0.1
predict(arima.yung.0.0.1,n.ahead = 5)





#용산구
yong.ts <- ts(yong)
plot(yong.ts, main="용산구")

yong.ts_ma<-sma(yong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(yong.ts)
pacf(yong.ts)


auto.arima(yong)
arima.yong.0.0.1 <- arima(yong, order = c(0,0,1))
arima.yong.0.0.0 <- auto.arima(yong)
arima.yong.0.0.1$aic  
arima.yong.0.0.0$aic #더 작음

arima.yong.0.0.0
predict(arima.yong.0.0.0,n.ahead = 5)



#은평구
eun.ts <- ts(eun)
plot(eun.ts, main="은평구")

eun.ts_ma<-sma(eun.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(eun.ts)
pacf(eun.ts)


auto.arima(eun)
arima.eun.2.0.1 <- arima(eun, order = c(2,0,1))
arima.eun.0.0.0 <- auto.arima(eun)
arima.eun.2.0.1$aic  #더 작음
arima.eun.0.0.0$aic 

arima.eun.2.0.1
predict(arima.eun.2.0.1,n.ahead = 5)



#종로구
jong.ts <- ts(jong)
plot(jong.ts, main="종로구")

jong.ts_ma<-sma(jong.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(jong.ts)
pacf(jong.ts)


auto.arima(jong)
arima.jong.0.0.1 <- arima(jong, order = c(0,0,1))
arima.jong.0.0.0 <- auto.arima(jong)
arima.jong.0.0.1$aic  #더 작음
arima.jong.0.0.0$aic 

arima.jong.0.0.1
predict(arima.jong.0.0.1,n.ahead = 5)




#중구
jung.ts <- ts(jung)
plot(jung.ts, main="중구")

jung.ts_ma<-sma(jung.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(jung.ts)
pacf(jung.ts)


auto.arima(jung)
arima.jung.0.0.1 <- arima(jung, order = c(0,0,1))
arima.jung.0.0.0 <- auto.arima(jung)
arima.jung.0.0.1$aic  #더 작음
arima.jung.0.0.0$aic 

arima.jung.0.0.1
predict(arima.jung.0.0.1,n.ahead = 5)



#중랑구
jlang.ts <- ts(jlang)
plot(jlang.ts, main="중랑구")

jlang.ts_ma<-sma(jlang.ts,n=3) #3회의 관측치에 대한 이동평균 값


acf(jlang.ts)
pacf(jlang.ts)


auto.arima(jlang)
arima.jlang.0.0.1 <- arima(jlang, order = c(0,0,1))
arima.jlang.0.0.0 <- auto.arima(jlang)
arima.jlang.0.0.1$aic  #더 작음
arima.jlang.0.0.0$aic 

arima.jlang.0.0.1
predict(arima.jlang.0.0.1,n.ahead = 5)


