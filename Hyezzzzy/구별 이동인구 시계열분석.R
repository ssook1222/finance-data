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

#강서구
gangseo<-move %>% filter(역명 %in% c('방화', '개화산', '김포공항', '송정', '마곡', '발산', '우장산', '화곡', '까치산', '염창', '등촌', '증미', '가양', '양천향교', '마곡나루', '신방화', '공항시장', '개화'))
gangseo<-gangseo %>% 
  dplyr::mutate(지역구 = '강서구')


#양천구
yang<-move %>% filter(역명 %in% c('신목동', '목동', '오목교', '신정네거리', '양천구청', '신정(은행정)'))
yang<-yang %>% 
  dplyr::mutate(지역구 = '양천구')


#금천구
keum<-move %>% filter(역명 %in% c('가산디지털단지', '독산', '금천구청'))
keum<-keum %>% 
  dplyr::mutate(지역구 = '금천구')


#관악구
kwan<-move %>% filter(역명 %in% c('신림', '봉천', '서울대입구', '낙성대'))
kwan<-kwan %>% 
  dplyr::mutate(지역구 = '관악구')


#은평구
eun<-move %>% filter(역명 %in% c('독바위', '구파발', '연신내', '불광', '녹번', '역촌', '구산', '응암', '새절(신사)','증산(명지대앞)', '수색', '디지털미디어시티'))
eun<-eun %>% 
  dplyr::mutate(지역구 = '은평구')


#구로구
guro<-move %>% filter(역명 %in% c('온수(성공회대입구)', '천왕', '오류동', '개봉', '구일', '구로', '남구로', '대림(구로구청)', '구로디지털단지','신도림', '도림천'))
guro<-guro %>% 
  dplyr::mutate(지역구 = '구로구')


#도봉구
dobong<-move %>% filter(역명 %in% c('도봉산', '도봉', '방학', '창동', '녹천', '쌍문'))
dobong<-dobong %>% 
  dplyr::mutate(지역구 = '도봉구')


#중랑구
jlang<-move %>% filter(역명 %in% c('먹골', '중화', '상봉(시외버스터미널)', '중량', '망우', '양원', '신내', '봉화산(서울의료원)', '면목', '사가정', '용마산'))
jlang<-jlang %>% 
  dplyr::mutate(지역구 = '중랑구')


#강동구
gangdong<-move %>% filter(역명 %in% c('강동구청', '천호(풍납토성)', '암사', '강동', '길동', '굽은다리(강동구민회관앞)', '명일', '고덕', '상일동', '강일', '둔촌동', '둔촌오류','중앙보훈병원'))
gangdong<-gangdong %>% 
  dplyr::mutate(지역구 = '강동구')


#용산구
yong<-move %>% filter(역명 %in% c('서울역', '숙대입구(갈월)', '녹사평(용산구청)', '이태원', '한강진', '한남', '서빙고', '이촌(국립중앙박물관)', '용산', '남영', '효창공원앞', '삼각지','신용산'))
yong<-yong %>% 
  dplyr::mutate(지역구 = '용산구')


#광진구
kwang<-move %>% filter(역명 %in% c('중곡', '군자(능동)', '어린이대공원', '아차산(어린이대공원후문)', '광나루(장신대)', '건대입구', '구의(광진구청)', '강변(동서울터미널)', '뚝섬유원지'))
kwang<-kwang %>% 
  dplyr::mutate(지역구 = '광진구')


#동대문구
moon<-move %>% filter(역명 %in% c('신설동', '용두(동대문구청)', '제기동', '청량리(서울시립대입구)', '회기', '외대앞', '신이문', '왕십리(성동구청)'))
moon<-moon %>% 
  dplyr::mutate(지역구 = '동대문구')


#노원구
noone<-move %>% filter(역명 %in% c('석계', '광운대', '월계', '태릉입구', '화랑대(서울여대입구)', '공릉(서울과학기술대)', '하계', '중계', '노원', '상계','당고개', '마들', '수락산'))
noone<-noone %>% 
  dplyr::mutate(지역구 = '노원구')


#강북구
kbook<-move %>% filter(역명 %in% c('솔샘', '삼양사거리', '삼양', '회계', '가오리', '4.19민주묘지', '솔밭공원', '북한산우이', '수유(강북구청)', '미아','미아사거리'))
kbook<-kbook %>% 
  dplyr::mutate(지역구 = '강북구')


#서대문구
seo<-move %>% filter(역명 %in% c('홍제', '무악재', '서대문', '충정로(경기대입구)', '신촌(경의중앙선)', '신촌', '가좌'))
seo<-seo %>% 
  dplyr::mutate(지역구 = '서대문구')


#성북구
seong<-move %>% filter(역명 %in% c('북한산보국문', '정릉', '성신여대입구', '길음', '한성대입구', '보문', '안암(고대병원앞)', '고려대(종암)','월곡(동덕여대)','상월곡(한국과학기술연구원)', '돌곶이'))
seong<-seong %>% 
  dplyr::mutate(지역구 = '성북구')


#종로구
jong<-move %>% filter(역명 %in% c('독립문', '경복궁(정부서울청사)', '안국', '종로3가', '광화문', '종각', '종로5가', '동대문','동묘앞','혜화', '창신'))
jong<-jong %>% 
  dplyr::mutate(지역구 = '종로구')


#중구
jung<-move %>% filter(역명 %in% c('시청', '을지로입구', '을지로3가', '을지로4가', '동대문역사문화공원', '신당', '청구', '약수','버티고개','동대입구', '충무로', '명동', '회현(남대문시장)'))
jung<-jung %>% 
  dplyr::mutate(지역구 = '중구')


#마포구
mapo<-move %>% filter(역명 %in% c('월드컵경기장', '마포구청', '홍대입구', '망원', '합정', '상수', '광흥창(서강)', '대흥(서강대앞)','마포','공덕', '애오개', '이대', '아현'))
mapo<-mapo %>% 
  dplyr::mutate(지역구 = '마포구')


#영등포구
yung<-move %>% filter(역명 %in% c('국회의사당', '여의나루', '샛강', '여의도', '대방', '신길', '영등포', '신풍','영등포시장','문래', '영등포구청', '당산', '양평', '선유도'))
yung<-yung %>% 
  dplyr::mutate(지역구 = '영등포구')


#송파구
song<-move %>% filter(역명 %in% c('잠실나루', '몽촌토성(평화의문)', '잠실(송파구청)', '잠실새내', '종합운동장', '삼전', '석촌고분', '석촌','송파','가락시장', '문정', '장지', '복정', '경찰병원','오금','방이','개롱','거여','마천','올림픽공원','한성백제','송파나루'))
song<-song %>% 
  dplyr::mutate(지역구 = '송파구')


#강남구
gnam<-move %>% filter(역명 %in% c('압구정로데오', '청담', '강남구청', '학동', '논현', '압구정', '신사', '잠원','신논현','언주', '선정릉', '삼성중앙','봉은사', '삼성(무역센터)', '선릉', '역삼', '강남', '한티', '매봉', '도곡', '대치', '학여울', '대청', '일원', '수서', '대모산입구', '개포동', '구룡')) 
gnam<-gnam %>% 
  dplyr::mutate(지역구 = '강남구')


#서초구
scho<-move %>% filter(역명 %in% c('신반포', '구반포', '반포', '고속터미널', '사평', '내방', '총신대입구','방배', '서초', '교대(법원.검찰청)', '남부터미널', '양재(서초구청)', '양재시민의숲', '청계산입구', '남태령')) 
scho<-scho %>% 
  dplyr::mutate(지역구 = '서초구')


#성동구
sdong<-move %>% filter(역명 %in% c('상왕십리', '왕십리', '마장', '신답', '용답', '뚝섬', '한양대', '상수', '서울숲', '응봉', '옥수', '행당', '신금호'))
sdong<-sdong %>% 
  dplyr::mutate(지역구 = '성동구')


#동작구
djak<-move %>% filter(역명 %in% c('노량진', '노들', '흑석(중앙대입구)', '동작(현충원)', '보라매', '신대방삼거리', '장승배기', '상도','숭실대입구', '남성', '신대방', '사당'))
djak<-djak %>% 
  dplyr::mutate(지역구 = '동작구')


#데이터프레임 합치기
subway<-rbind(gangseo, yang, keum, kwan, eun, guro, dobong, jlang, gangdong, yong, kwang, moon, noone, kbook, seo, seong, jong, jung, mapo, yung, song, gnam, scho, sdong, djak)

agg_sub <- aggregate(총계~기준년월+지역구, subway, mean)
head(agg_sub)

#구별 나누기
sub_gangseo<-agg_sub %>% filter(지역구=='강서구')
sub_yang<-agg_sub %>% filter(지역구=='양천구')
sub_keum<-agg_sub %>% filter(지역구=='금천구')
sub_kwan<-agg_sub %>% filter(지역구=='관악구')
sub_eun<-agg_sub %>% filter(지역구=='은평구')
sub_guro<-agg_sub %>% filter(지역구=='구로구')
sub_dobong<-agg_sub %>% filter(지역구=='도봉구')
sub_jlang<-agg_sub %>% filter(지역구=='중랑구')
sub_gangdong<-agg_sub %>% filter(지역구=='강동구')
sub_yong<-agg_sub %>% filter(지역구=='용산구')
sub_kwang<-agg_sub %>% filter(지역구=='광진구')
sub_moon<-agg_sub %>% filter(지역구=='동대문구')
sub_noone<-agg_sub %>% filter(지역구=='노원구')
sub_kbook<-agg_sub %>% filter(지역구=='강북구')
sub_seo<-agg_sub %>% filter(지역구=='서대문구')
sub_seong<-agg_sub %>% filter(지역구=='성북구')
sub_jong<-agg_sub %>% filter(지역구=='종로구')
sub_jung<-agg_sub %>% filter(지역구=='중구')
sub_mapo<-agg_sub %>% filter(지역구=='마포구')
sub_yung<-agg_sub %>% filter(지역구=='영등포구')
sub_song<-agg_sub %>% filter(지역구=='송파구')
sub_gnam<-agg_sub %>% filter(지역구=='강남구')
sub_scho<-agg_sub %>% filter(지역구=='서초구')
sub_sdong<-agg_sub %>% filter(지역구=='성동구')
sub_djak<-agg_sub %>% filter(지역구=='동작구')



#강남구
gnam.ts <- ts(sub_gnam$총계)
plot(gnam.ts, main="강남구")

gnam.ts_ma<-sma(gnam.ts,n=3) #3회의 관측치에 대한 이동평균 값


gnam.tsdiff1 <- diff(gnam.ts, differences = 1)
gnam.tsdiff2 <- diff(gnam.ts, differences = 2)
gnam.tsdiff3 <- diff(gnam.ts, differences = 3)

plot.ts(gnam.tsdiff1)
plot.ts(gnam.tsdiff2)
plot.ts(gnam.tsdiff3)
acf(gnam.tsdiff3)
pacf(gnam.tsdiff3)


auto.arima(sub_gnam$총계)
arima.gnam.1.3.2 <- arima(sub_gnam$총계, order = c(1,3,2))
arima.gnam.0.1.0 <- auto.arima(sub_gnam$총계)
arima.gnam.1.3.2$aic #더 작음
arima.gnam.0.1.0$aic

gnam.forecast <- forecast(arima.gnam.1.3.2)
gnam.forecast


#강동구
gangdong.ts <- ts(sub_gangdong$총계)
plot(gangdong.ts, main="강동구")

gangdong.ts_ma<-sma(gangdong.ts,n=3) #3회의 관측치에 대한 이동평균 값


gangdong.tsdiff1 <- diff(gangdong.ts, differences = 1)
gangdong.tsdiff2 <- diff(gangdong.ts, differences = 2)
gangdong.tsdiff3 <- diff(gangdong.ts, differences = 3)

plot.ts(gangdong.tsdiff1)
plot.ts(gangdong.tsdiff2)
plot.ts(gangdong.tsdiff3)
acf(gangdong.tsdiff1)
pacf(gangdong.tsdiff1)


auto.arima(sub_gangdong$총계)
arima.gangdong.0.1.1 <- arima(sub_gangdong$총계, order = c(0,1,1))
arima.gangdong.0.1.0 <- auto.arima(sub_gangdong$총계)
arima.gangdong.0.1.1$aic 
arima.gangdong.0.1.0$aic #더 작음

gangdong.forecast <- forecast(arima.gangdong.0.1.0)
gangdong.forecast


#강북구
kbook.ts <- ts(sub_kbook$총계)
plot(kbook.ts, main="강북구")

kbook.ts_ma<-sma(kbook.ts,n=3) #3회의 관측치에 대한 이동평균 값


kbook.tsdiff1 <- diff(kbook.ts, differences = 1)
kbook.tsdiff2 <- diff(kbook.ts, differences = 2)
kbook.tsdiff3 <- diff(kbook.ts, differences = 3)

plot.ts(kbook.tsdiff1)
plot.ts(kbook.tsdiff2)
plot.ts(kbook.tsdiff3)
acf(kbook.tsdiff1)
pacf(kbook.tsdiff1)


auto.arima(sub_kbook$총계)
arima.kbook.0.1.1 <- arima(sub_kbook$총계, order = c(0,1,1))
arima.kbook.0.1.0 <- auto.arima(sub_kbook$총계)
arima.kbook.0.1.1$aic 
arima.kbook.0.1.0$aic #더 작음

kbook.forecast <- forecast(arima.kbook.0.1.0)
kbook.forecast


#강서구
gangseo.ts <- ts(sub_gangseo$총계)
plot(gangseo.ts, main="강서구")

gangseo.ts_ma<-sma(gangseo.ts,n=3) #3회의 관측치에 대한 이동평균 값


gangseo.tsdiff1 <- diff(gangseo.ts, differences = 1)
gangseo.tsdiff2 <- diff(gangseo.ts, differences = 2)
gangseo.tsdiff3 <- diff(gangseo.ts, differences = 3)

plot.ts(gangseo.tsdiff1)
plot.ts(gangseo.tsdiff2)
plot.ts(gangseo.tsdiff3)
acf(gangseo.tsdiff2)
pacf(gangseo.tsdiff2)


auto.arima(sub_gangseo$총계)
arima.gangseo.0.2.1 <- arima(sub_gangseo$총계, order = c(0,2,1))
arima.gangseo.0.1.0 <- auto.arima(sub_gangseo$총계)
arima.gangseo.0.2.1$aic  #더 작음
arima.gangseo.0.1.0$aic

gangseo.forecast <- forecast(arima.gangseo.0.2.1)
gangseo.forecast


#관악구
kwan.ts <- ts(sub_kwan$총계)
plot(kwan.ts, main="관악구")

kwan.ts_ma<-sma(kwan.ts,n=3) #3회의 관측치에 대한 이동평균 값


kwan.tsdiff1 <- diff(kwan.ts, differences = 1)
kwan.tsdiff2 <- diff(kwan.ts, differences = 2)
kwan.tsdiff3 <- diff(kwan.ts, differences = 3)

plot.ts(kwan.tsdiff1)
plot.ts(kwan.tsdiff2)
plot.ts(kwan.tsdiff3)
acf(kwan.tsdiff2)
pacf(kwan.tsdiff2)


auto.arima(sub_kwan$총계)
arima.kwan.0.2.1 <- arima(sub_kwan$총계, order = c(0,2,1))
arima.kwan.0.1.0 <- auto.arima(sub_kwan$총계)
arima.kwan.0.2.1$aic  #더 작음
arima.kwan.0.1.0$aic  

kwan.forecast <- forecast(arima.kwan.0.2.1)
kwan.forecast


#광진구
kwang.ts <- ts(sub_kwang$총계)
plot(kwang.ts, main="광진구")

kwang.ts_ma<-sma(kwang.ts,n=3) #3회의 관측치에 대한 이동평균 값


kwang.tsdiff1 <- diff(kwang.ts, differences = 1)
kwang.tsdiff2 <- diff(kwang.ts, differences = 2)
kwang.tsdiff3 <- diff(kwang.ts, differences = 3)

plot.ts(kwang.tsdiff1)
plot.ts(kwang.tsdiff2)
plot.ts(kwang.tsdiff3)
acf(kwang.tsdiff3)
pacf(kwang.tsdiff3)


auto.arima(sub_kwang$총계)
arima.kwang.0.3.1 <- arima(sub_kwang$총계, order = c(0,3,1))
arima.kwang.0.1.0 <- auto.arima(sub_kwang$총계)
arima.kwang.0.3.1$aic  #더 작음
arima.kwang.0.1.0$aic  

kwang.forecast <- forecast(arima.kwang.0.3.1)
kwang.forecast



#구로구
guro.ts <- ts(sub_guro$총계)
plot(guro.ts, main="구로구")

guro.ts_ma<-sma(guro.ts,n=3) #3회의 관측치에 대한 이동평균 값


guro.tsdiff1 <- diff(guro.ts, differences = 1)
guro.tsdiff2 <- diff(guro.ts, differences = 2)
guro.tsdiff3 <- diff(guro.ts, differences = 3)

plot.ts(guro.tsdiff1)
plot.ts(guro.tsdiff2)
plot.ts(guro.tsdiff3)
acf(guro.tsdiff3)
pacf(guro.tsdiff3)


auto.arima(sub_guro$총계)
arima.guro.0.3.1 <- arima(sub_guro$총계, order = c(0,3,1))
arima.guro.0.1.0 <- auto.arima(sub_guro$총계)
arima.guro.0.3.1$aic  #더 작음
arima.guro.0.1.0$aic  

guro.forecast <- forecast(arima.guro.0.3.1)
guro.forecast


#금천구
keum.ts <- ts(sub_keum$총계)
plot(keum.ts, main="금천구")

keum.ts_ma<-sma(keum.ts,n=3) #3회의 관측치에 대한 이동평균 값


keum.tsdiff1 <- diff(keum.ts, differences = 1)
keum.tsdiff2 <- diff(keum.ts, differences = 2)
keum.tsdiff3 <- diff(keum.ts, differences = 3)

plot.ts(keum.tsdiff1)
plot.ts(keum.tsdiff2)
plot.ts(keum.tsdiff3)
acf(keum.tsdiff1)
pacf(keum.tsdiff1)


auto.arima(sub_keum$총계)
arima.keum.0.1.1 <- arima(sub_keum$총계, order = c(0,1,1))
arima.keum.0.1.0 <- auto.arima(sub_keum$총계)
arima.keum.0.1.1$aic  
arima.keum.0.1.0$aic  #더 작음

keum.forecast <- forecast(arima.keum.0.1.0)
keum.forecast



#노원구
noone.ts <- ts(sub_noone$총계)
plot(noone.ts, main="노원구")

noone.ts_ma<-sma(noone.ts,n=3) #3회의 관측치에 대한 이동평균 값


noone.tsdiff1 <- diff(noone.ts, differences = 1)
noone.tsdiff2 <- diff(noone.ts, differences = 2)
noone.tsdiff3 <- diff(noone.ts, differences = 3)

plot.ts(noone.tsdiff1)
plot.ts(noone.tsdiff2)
plot.ts(noone.tsdiff3)
acf(noone.tsdiff2)
pacf(noone.tsdiff2)


auto.arima(sub_noone$총계)
arima.noone.0.2.1 <- arima(sub_noone$총계, order = c(0,2,1))
arima.noone.0.1.0 <- auto.arima(sub_noone$총계)
arima.noone.0.2.1$aic  #더 작음
arima.noone.0.1.0$aic  

noone.forecast <- forecast(arima.noone.0.2.1)
noone.forecast



#도봉구
dobong.ts <- ts(sub_dobong$총계)
plot(dobong.ts, main="도봉구")

dobong.ts_ma<-sma(dobong.ts,n=3) #3회의 관측치에 대한 이동평균 값


dobong.tsdiff1 <- diff(dobong.ts, differences = 1)
dobong.tsdiff2 <- diff(dobong.ts, differences = 2)
dobong.tsdiff3 <- diff(dobong.ts, differences = 3)

plot.ts(dobong.tsdiff1)
plot.ts(dobong.tsdiff2)
plot.ts(dobong.tsdiff3)
acf(dobong.tsdiff2)
pacf(dobong.tsdiff2)


auto.arima(sub_dobong$총계)
arima.dobong.0.2.1 <- arima(sub_dobong$총계, order = c(0,2,1))
arima.dobong.0.1.0 <- auto.arima(sub_dobong$총계)
arima.dobong.0.2.1$aic  #더 작음
arima.dobong.0.1.0$aic  

dobong.forecast <- forecast(arima.dobong.0.2.1)
dobong.forecast




#동대문구
moon.ts <- ts(sub_moon$총계)
plot(moon.ts, main="동대문구")

moon.ts_ma<-sma(moon.ts,n=3) #3회의 관측치에 대한 이동평균 값


moon.tsdiff1 <- diff(moon.ts, differences = 1)
moon.tsdiff2 <- diff(moon.ts, differences = 2)
moon.tsdiff3 <- diff(moon.ts, differences = 3)

plot.ts(moon.tsdiff1)
plot.ts(moon.tsdiff2)
plot.ts(moon.tsdiff3)
acf(moon.tsdiff3)
pacf(moon.tsdiff3)


auto.arima(sub_moon$총계)
arima.moon.0.3.1 <- arima(sub_moon$총계, order = c(0,3,1))
arima.moon.0.1.0 <- auto.arima(sub_moon$총계)
arima.moon.0.3.1$aic  #더 작음
arima.moon.0.1.0$aic  

moon.forecast <- forecast(arima.moon.0.3.1)
moon.forecast




#동작구
djak.ts <- ts(sub_djak$총계)
plot(djak.ts, main="동작구")

djak.ts_ma<-sma(djak.ts,n=3) #3회의 관측치에 대한 이동평균 값


djak.tsdiff1 <- diff(djak.ts, differences = 1)
djak.tsdiff2 <- diff(djak.ts, differences = 2)
djak.tsdiff3 <- diff(djak.ts, differences = 3)

plot.ts(djak.tsdiff1)
plot.ts(djak.tsdiff2)
plot.ts(djak.tsdiff3)
acf(djak.tsdiff3)
pacf(djak.tsdiff3)


auto.arima(sub_djak$총계)
arima.djak.0.3.1 <- arima(sub_djak$총계, order = c(0,3,1))
arima.djak.0.1.0 <- auto.arima(sub_djak$총계)
arima.djak.0.3.1$aic  #더 작음
arima.djak.0.1.0$aic  

djak.forecast <- forecast(arima.djak.0.3.1)
djak.forecast



#마포구
mapo.ts <- ts(sub_mapo$총계)
plot(mapo.ts, main="마포구")

mapo.ts_ma<-sma(mapo.ts,n=3) #3회의 관측치에 대한 이동평균 값


mapo.tsdiff1 <- diff(mapo.ts, differences = 1)
mapo.tsdiff2 <- diff(mapo.ts, differences = 2)
mapo.tsdiff3 <- diff(mapo.ts, differences = 3)

plot.ts(mapo.tsdiff1)
plot.ts(mapo.tsdiff2)
plot.ts(mapo.tsdiff3)
acf(mapo.tsdiff2)
pacf(mapo.tsdiff2)


auto.arima(sub_mapo$총계)
arima.mapo.0.2.1 <- arima(sub_mapo$총계, order = c(0,2,1))
arima.mapo.0.1.0 <- auto.arima(sub_mapo$총계)
arima.mapo.0.2.1$aic  #더 작음
arima.mapo.0.1.0$aic  

mapo.forecast <- forecast(arima.mapo.0.2.1)
mapo.forecast





#서대문구
seo.ts <- ts(sub_seo$총계)
plot(seo.ts, main="서대문구")

seo.ts_ma<-sma(seo.ts,n=3) #3회의 관측치에 대한 이동평균 값


seo.tsdiff1 <- diff(seo.ts, differences = 1)
seo.tsdiff2 <- diff(seo.ts, differences = 2)
seo.tsdiff3 <- diff(seo.ts, differences = 3)

plot.ts(seo.tsdiff1)
plot.ts(seo.tsdiff2)
plot.ts(seo.tsdiff3)
acf(seo.tsdiff2)
pacf(seo.tsdiff2)


auto.arima(sub_seo$총계)
arima.seo.0.2.1 <- arima(sub_seo$총계, order = c(0,2,1))
arima.seo.0.1.0 <- auto.arima(sub_seo$총계)
arima.seo.0.2.1$aic  #더 작음
arima.seo.0.1.0$aic  

seo.forecast <- forecast(arima.seo.0.2.1)
seo.forecast




#서초구
scho.ts <- ts(sub_scho$총계)
plot(scho.ts, main="서초구")

scho.ts_ma<-sma(scho.ts,n=3) #3회의 관측치에 대한 이동평균 값


scho.tsdiff1 <- diff(scho.ts, differences = 1)
scho.tsdiff2 <- diff(scho.ts, differences = 2)
scho.tsdiff3 <- diff(scho.ts, differences = 3)

plot.ts(scho.tsdiff1)
plot.ts(scho.tsdiff2)
plot.ts(scho.tsdiff3)
acf(scho.tsdiff2)
pacf(scho.tsdiff2)


auto.arima(sub_scho$총계)
arima.scho.0.2.1 <- arima(sub_scho$총계, order = c(0,2,1))
arima.scho.0.1.0 <- auto.arima(sub_scho$총계)
arima.scho.0.2.1$aic  #더 작음
arima.scho.0.1.0$aic  

scho.forecast <- forecast(arima.scho.0.2.1)
scho.forecast





#성동구
sdong.ts <- ts(sub_sdong$총계)
plot(sdong.ts, main="성동구")

sdong.ts_ma<-sma(sdong.ts,n=3) #3회의 관측치에 대한 이동평균 값


sdong.tsdiff1 <- diff(sdong.ts, differences = 1)
sdong.tsdiff2 <- diff(sdong.ts, differences = 2)
sdong.tsdiff3 <- diff(sdong.ts, differences = 3)

plot.ts(sdong.tsdiff1)
plot.ts(sdong.tsdiff2)
plot.ts(sdong.tsdiff3)
acf(sdong.tsdiff2)
pacf(sdong.tsdiff2)


auto.arima(sub_sdong$총계)
arima.sdong.0.2.1 <- arima(sub_sdong$총계, order = c(0,2,1))
arima.sdong.0.1.0 <- auto.arima(sub_sdong$총계)
arima.sdong.0.2.1$aic  #더 작음
arima.sdong.0.1.0$aic  

sdong.forecast <- forecast(arima.sdong.0.2.1)
sdong.forecast





#성북구
seong.ts <- ts(sub_seong$총계)
plot(seong.ts, main="성북구")

seong.ts_ma<-sma(seong.ts,n=3) #3회의 관측치에 대한 이동평균 값


seong.tsdiff1 <- diff(seong.ts, differences = 1)
seong.tsdiff2 <- diff(seong.ts, differences = 2)
seong.tsdiff3 <- diff(seong.ts, differences = 3)

plot.ts(seong.tsdiff1)
plot.ts(seong.tsdiff2)
plot.ts(seong.tsdiff3)
acf(seong.tsdiff2)
pacf(seong.tsdiff2)


auto.arima(sub_seong$총계)
arima.seong.0.2.2 <- arima(sub_seong$총계, order = c(0,2,2))
arima.seong.0.1.0 <- auto.arima(sub_seong$총계)
arima.seong.0.2.2$aic  #더 작음
arima.seong.0.1.0$aic  

seong.forecast <- forecast(arima.seong.0.2.2)
seong.forecast




#송파구
song.ts <- ts(sub_song$총계)
plot(song.ts, main="송파구")

song.ts_ma<-sma(song.ts,n=3) #3회의 관측치에 대한 이동평균 값


song.tsdiff1 <- diff(song.ts, differences = 1)
song.tsdiff2 <- diff(song.ts, differences = 2)
song.tsdiff3 <- diff(song.ts, differences = 3)

plot.ts(song.tsdiff1)
plot.ts(song.tsdiff2)
plot.ts(song.tsdiff3)
acf(song.tsdiff2)
pacf(song.tsdiff2)


auto.arima(sub_song$총계)
arima.song.0.2.1 <- arima(sub_song$총계, order = c(0,2,1))
arima.song.0.1.0 <- auto.arima(sub_song$총계)
arima.song.0.2.1$aic  #더 작음
arima.song.0.1.0$aic  

song.forecast <- forecast(arima.song.0.2.1)
song.forecast




#양천구
yang.ts <- ts(sub_yang$총계)
plot(yang.ts, main="양천구")

yang.ts_ma<-sma(yang.ts,n=3) #3회의 관측치에 대한 이동평균 값


yang.tsdiff1 <- diff(yang.ts, differences = 1)
yang.tsdiff2 <- diff(yang.ts, differences = 2)
yang.tsdiff3 <- diff(yang.ts, differences = 3)

plot.ts(yang.tsdiff1)
plot.ts(yang.tsdiff2)
plot.ts(yang.tsdiff3)
acf(yang.tsdiff2)
pacf(yang.tsdiff2)


auto.arima(sub_yang$총계)
arima.yang.0.2.1 <- arima(sub_yang$총계, order = c(0,2,1))
arima.yang.0.1.0 <- auto.arima(sub_yang$총계)
arima.yang.0.2.1$aic  #더 작음
arima.yang.0.1.0$aic  

yang.forecast <- forecast(arima.yang.0.2.1)
yang.forecast





#영등포구
yung.ts <- ts(sub_yung$총계)
plot(yung.ts, main="영등포구")

yung.ts_ma<-sma(yung.ts,n=3) #3회의 관측치에 대한 이동평균 값


yung.tsdiff1 <- diff(yung.ts, differences = 1)
yung.tsdiff2 <- diff(yung.ts, differences = 2)
yung.tsdiff3 <- diff(yung.ts, differences = 3)

plot.ts(yung.tsdiff1)
plot.ts(yung.tsdiff2)
plot.ts(yung.tsdiff3)
acf(yung.tsdiff2)
pacf(yung.tsdiff2)


auto.arima(sub_yung$총계)
arima.yung.0.2.1 <- arima(sub_yung$총계, order = c(0,2,1))
arima.yung.0.1.0 <- auto.arima(sub_yung$총계)
arima.yung.0.2.1$aic  #더 작음
arima.yung.0.1.0$aic  

yung.forecast <- forecast(arima.yung.0.2.1)
yung.forecast





#용산구
yong.ts <- ts(sub_yong$총계)
plot(yong.ts, main="용산구")

yong.ts_ma<-sma(yong.ts,n=3) #3회의 관측치에 대한 이동평균 값


yong.tsdiff1 <- diff(yong.ts, differences = 1)
yong.tsdiff2 <- diff(yong.ts, differences = 2)
yong.tsdiff3 <- diff(yong.ts, differences = 3)

plot.ts(yong.tsdiff1)
plot.ts(yong.tsdiff2)
plot.ts(yong.tsdiff3)
acf(yong.tsdiff2)
pacf(yong.tsdiff2)


auto.arima(sub_yong$총계)
arima.yong.0.2.1 <- arima(sub_yong$총계, order = c(0,2,1))
arima.yong.0.1.0 <- auto.arima(sub_yong$총계)
arima.yong.0.2.1$aic  #더 작음
arima.yong.0.1.0$aic  

yong.forecast <- forecast(arima.yong.0.2.1)
yong.forecast





#은평구
eun.ts <- ts(sub_eun$총계)
plot(eun.ts, main="은평구")

eun.ts_ma<-sma(eun.ts,n=3) #3회의 관측치에 대한 이동평균 값


eun.tsdiff1 <- diff(eun.ts, differences = 1)
eun.tsdiff2 <- diff(eun.ts, differences = 2)
eun.tsdiff3 <- diff(eun.ts, differences = 3)

plot.ts(eun.tsdiff1)
plot.ts(eun.tsdiff2)
plot.ts(eun.tsdiff3)
acf(eun.tsdiff2)
pacf(eun.tsdiff2)


auto.arima(sub_eun$총계)
arima.eun.0.2.1 <- arima(sub_eun$총계, order = c(0,2,1))
arima.eun.0.1.0 <- auto.arima(sub_eun$총계)
arima.eun.0.2.1$aic  #더 작음
arima.eun.0.1.0$aic  

eun.forecast <- forecast(arima.eun.0.2.1)
eun.forecast



#종로구
jong.ts <- ts(sub_jong$총계)
plot(jong.ts, main="종로구")

jong.ts_ma<-sma(jong.ts,n=3) #3회의 관측치에 대한 이동평균 값


jong.tsdiff1 <- diff(jong.ts, differences = 1)
jong.tsdiff2 <- diff(jong.ts, differences = 2)
jong.tsdiff3 <- diff(jong.ts, differences = 3)

plot.ts(jong.tsdiff1)
plot.ts(jong.tsdiff2)
plot.ts(jong.tsdiff3)
acf(jong.tsdiff3)
pacf(jong.tsdiff3)


auto.arima(sub_jong$총계)
arima.jong.1.3.2 <- arima(sub_jong$총계, order = c(1,3,2))
arima.jong.0.1.0 <- auto.arima(sub_jong$총계)
arima.jong.1.3.2$aic  #더 작음
arima.jong.0.1.0$aic  

jong.forecast <- forecast(arima.jong.1.3.2)
jong.forecast




#중구
jung.ts <- ts(sub_jung$총계)
plot(jung.ts, main="중구")

jung.ts_ma<-sma(jung.ts,n=3) #3회의 관측치에 대한 이동평균 값


jung.tsdiff1 <- diff(jung.ts, differences = 1)
jung.tsdiff2 <- diff(jung.ts, differences = 2)
jung.tsdiff3 <- diff(jung.ts, differences = 3)

plot.ts(jung.tsdiff1)
plot.ts(jung.tsdiff2)
plot.ts(jung.tsdiff3)
acf(jung.tsdiff2)
pacf(jung.tsdiff2)


auto.arima(sub_jung$총계)
arima.jung.0.2.1 <- arima(sub_jung$총계, order = c(0,2,1))
arima.jung.0.1.0 <- auto.arima(sub_jung$총계)
arima.jung.0.2.1$aic  #더 작음
arima.jung.0.1.0$aic  

jung.forecast <- forecast(arima.jung.0.2.1)
jung.forecast




#중랑구
jlang.ts <- ts(sub_jlang$총계)
plot(jlang.ts, main="중랑구")

jlang.ts_ma<-sma(jlang.ts,n=3) #3회의 관측치에 대한 이동평균 값


jlang.tsdiff1 <- diff(jlang.ts, differences = 1)
jlang.tsdiff2 <- diff(jlang.ts, differences = 2)
jlang.tsdiff3 <- diff(jlang.ts, differences = 3)

plot.ts(jlang.tsdiff1)
plot.ts(jlang.tsdiff2)
plot.ts(jlang.tsdiff3)
acf(jlang.tsdiff2)
pacf(jlang.tsdiff2)


auto.arima(sub_jlang$총계)
arima.jlang.0.2.1 <- arima(sub_jlang$총계, order = c(0,2,1))
arima.jlang.0.1.0 <- auto.arima(sub_jlang$총계)
arima.jlang.0.2.1$aic  #더 작음
arima.jlang.0.1.0$aic  

jlang.forecast <- forecast(arima.jlang.0.2.1)
jlang.forecast



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
acf(agg_move.tsdiff2)
pacf(agg_move.tsdiff2)


auto.arima(agg_move$총계)
arima.agg_move.0.2.1 <- arima(agg_move$총계, order = c(0,2,1))
arima.agg_move.0.1.0 <- auto.arima(agg_move$총계)
arima.agg_move.0.2.1$aic  #더 작음
arima.agg_move.0.1.0$aic  

agg_move.forecast <- forecast(arima.agg_move.0.2.1)
agg_move.forecast



