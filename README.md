# finance-data
금융데이터 경진대회 team repo
 '''
#install.packages("dplyr")
#library(dplyr)

mov20<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2020.csv")
mov20


#분류부터 기타까지 남기고 삭제
mov20<- mov20 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov20)

#행별 합계 구하기
mov20<-mov20 %>% mutate(합계20 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov20)

#구 별로만 보기
gu20<-mov20 %>% filter(분류 %in% c('강서구', '양천구', '구로구', '금천구', '영등포구', '동작구', '관악구', '서초구', '강남구', '송파구', '강동구', '마포구', '용산구', '중구', '성동구', '광진구', '서대문구', '은평구', '종로구', '성북구', '동대문구', '중랑구', '노원구', '강북구', '도봉구'))
gu20

gu20<- gu20 %>% select(분류, 합계20)
gu20

 '''

 '''
#install.packages("dplyr")
#library(dplyr)

mov19<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2019.csv")
mov19


#분류부터 기타까지 남기고 삭제
mov19<- mov19 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov19)

#행별 합계 구하기
mov19<-mov19 %>% mutate(합계19 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov19)

#구 별로만 보기
gu19<-mov19 %>% filter(분류 %in% c('강서구', '양천구', '구로구', '금천구', '영등포구', '동작구', '관악구', '서초구', '강남구', '송파구', '강동구', '마포구', '용산구', '중구', '성동구', '광진구', '서대문구', '은평구', '종로구', '성북구', '동대문구', '중랑구', '노원구', '강북구', '도봉구'))
gu19

gu19<- gu19 %>% select(분류,합계19)
gu19

#2019,2020 merge 후 barplot 만들기
move<-merge(gu19,gu20, by=c("분류"))
move


# 그래프 그리기
library(ggplot2)
#ggplot()+geom_bar(data=move, aes(x=분류, y=gu19, color="pink"),stat="identity")+geom_bar(data=move, aes(x=분류, y=gu20, color="red"),stat="identity")


 '''

 '''

#도보만 비교하기
#install.packages("dplyr")
#library(dplyr)

mov20<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2020.csv")
mov20


#분류부터 기타까지 남기고 삭제
mov20<- mov20 %>% select(분류, 도보)
head(mov20)

#구 별로만 보기
walk20<-mov20 %>% filter(분류 %in% c('강서구', '양천구', '구로구', '금천구', '영등포구', '동작구', '관악구', '서초구', '강남구', '송파구', '강동구', '마포구', '용산구', '중구', '성동구', '광진구', '서대문구', '은평구', '종로구', '성북구', '동대문구', '중랑구', '노원구', '강북구', '도봉구'))
walk20


mov19<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2019.csv")
mov19


#분류부터 기타까지 남기고 삭제
mov19<- mov19 %>% select(분류, 도보)
head(mov19)

#구 별로만 보기
walk19<-mov19 %>% filter(분류 %in% c('강서구', '양천구', '구로구', '금천구', '영등포구', '동작구', '관악구', '서초구', '강남구', '송파구', '강동구', '마포구', '용산구', '중구', '성동구', '광진구', '서대문구', '은평구', '종로구', '성북구', '동대문구', '중랑구', '노원구', '강북구', '도봉구'))
walk19



#2019,2020 merge 후 barplot 만들기

mov<-merge(walk19,walk20, by=c("분류"))
mov

  '''

 '''

#20대만 놓고 보기

mov20<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2020.csv")
mov20


#분류부터 기타까지 남기고 삭제
mov20<- mov20 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov20)

#나이대 별로만 보기
age20<-mov20 %>% filter(분류 %in% c('20대'))
age20

age20<-age20 %>% mutate(age20 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(age20)

age20<-age20 %>% select(분류, age20)
age20



mov19<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2019.csv")
mov19


#분류부터 기타까지 남기고 삭제
mov19<- mov19 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov19)

#나이대로만 보기
age19<-mov19 %>% filter(분류 %in% c('20대'))
age19

age19<-age19 %>% mutate(age19 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(age19)

age19<- age19 %>% select(분류, age19)
age19

#2019,2020 merge 후 barplot 만들기

movage<-merge(age19,age20, by=c("분류"))
movage


 '''

 '''
#도심 및 그 주변부로 보기

mov20<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2020.csv")
mov20


#분류부터 기타까지 남기고 삭제
mov20<- mov20 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov20)

#지역 별로만 보기
map20<-mov20 %>% filter(분류 %in% c('도심권', '동북권', '서북권', '서남권', '동남권'))
map20

map20<-map20 %>% mutate(map20 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(map20)

map20<-map20 %>% select(분류, map20)
map20



mov19<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\이동데이터 2019.csv")
mov19


#분류부터 기타까지 남기고 삭제
mov19<- mov19 %>% select(분류, 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(mov19)

#나이대로만 보기
map19<-mov19 %>% filter(분류 %in% c('도심권', '동북권', '서북권', '서남권', '동남권'))
map19

map19<-map19 %>% mutate(map19 = 도보, 자전거, 자전거.기타교통수단, 오토바이, 버스, 지하철.철도., 버스.지하철, 택시, 승용차, 승용차.버스, 승용차.지하철, 기타)
head(map19)

map19<- map19 %>% select(분류, map19)
map19

#2019,2020 merge 후 barplot 만들기

mapp<-merge(map19,map20, by=c("분류"))
mapp

 '''
