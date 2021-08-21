install.packages("dplyr")
library(dplyr)

# 기준년월 전처리 된 파일 데이터 추가
corona<-read.csv("./data/seoul_corona.csv", fileEncoding = "euc-kr", quote="")

# 사용할 데이터만 추출
corona<-corona %>% select(기준년월, 종로구.추가, 중구.추가, 용산구.추가, 성동구.추가, 광진구.추가,
                          동대문구.추가, 중랑구.추가, 성북구.추가, 강북구.추가, 도봉구.추가, 노원구.추가,      
                          은평구.추가, 서대문구.추가, 마포구.추가, 양천구.추가, 강서구.추가, 구로구.추가,
                          금천구.추가, 영등포구.추가, 동작구.추가, 관악구.추가, 서추구.추가, 강남구.추가,
                          강동구.추가, 송파구.추가,기타.추가
                          )
corona$기준년월
# 이상치 제거
corona$기준년월 <- ifelse(corona$기준년월=="1905-07",NA, corona$기준년월)
corona<-na.omit(corona)

# 구별로 세션 추출하기

송파구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(송파구.추가))

종로구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(종로구.추가))

중구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(중구.추가))

용산구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(용산구.추가))

성동구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(성동구.추가))

광진구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(광진구.추가))

동대문구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(동대문구.추가))

중랑구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(중랑구.추가))

성북구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(성북구.추가))

강북구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(강북구.추가))

도봉구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(도봉구.추가))

노원구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(노원구.추가))

은평구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(은평구.추가))

서대문구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(서대문구.추가))

마포구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(마포구.추가))

양천구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(양천구.추가))

강서구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(강서구.추가))

구로구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(구로구.추가))

금천구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(금천구.추가))

영등포구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(영등포구.추가))

동작구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(동작구.추가))

관악구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(관악구.추가))

서초구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(서추구.추가))

강남구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(강남구.추가))

강동구 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(강동구.추가))

기타 <- corona %>%
  group_by(기준년월) %>%
  summarise(sum=sum(기타.추가))

# 새로운 데이터 프레임 생성
corona_local <- data.frame(종로구=종로구,중구=중구,용산구=용산구,성동구=성동구,광진구=광진구,동대문구= 동대문구, 중랑구=중랑구, 성북구=성북구, 강북구=강북구,도봉구=도봉구,
                              노원구=노원구,은평구= 은평구,서대문구= 서대문구, 마포구=마포구, 양천구=양천구, 강서구=강서구,
                              구로구=구로구, 금천구=금천구, 영등포구=영등포구, 동작구=동작구, 관악구=관악구, 송파구=송파구,
                              서초구=서초구, 강남구=강남구, 강동구=강동구, 기타=기타,stringsAsFactors = FALSE)
# 데이터 전처리
corona_local<-corona_local %>% select(종로구.기준년월, 종로구.sum, 중구.sum, 용산구.sum, 성동구.sum, 광진구.sum,
                              동대문구.sum, 중랑구.sum, 성북구.sum, 강북구.sum, 도봉구.sum, 노원구.sum,      
                              은평구.sum, 서대문구.sum, 마포구.sum, 양천구.sum, 강서구.sum, 구로구.sum,
                              금천구.sum, 영등포구.sum, 동작구.sum, 관악구.sum, 서초구.sum, 강남구.sum,송파구.sum,
                              강동구.sum, 기타.sum
                              )
corona_local<-rename(corona_local,기준년월=종로구.기준년월)
View(corona_local)

#코로나 확진자가 급 증가하는 포인트체크를 위한 각 행간 차 계산
for(i in 1:length(corona_local)){
  광진구.dif<-corona_local$광진구.sum[i+1]-corona_local$광진구.sum[i]
  print(광진구.dif)
}

View(corona_local)
corona_local[ , "기타.dif"] <- c(  19,
                                 -6,
                                 -3,
                                 13,
                                 -13,
                                 157,
                                 -40,
                                 -47,
                                 113,
                                 1301,
                                 -769,
                                 -306,
                                 -188,
                                 69,
                                 -52,
                                 69,
                                 318,
                                 NA)
View(corona_local)

# 데이터 프레임 외부로 보내기
write.csv(corona_local,file="corona_diff.csv")
