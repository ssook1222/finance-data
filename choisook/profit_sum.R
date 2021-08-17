#라이브러리 설치
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)

#데이터 불러오기
enterprise<-read.csv("../financialData/ent.csv", fileEncoding = "utf-8", quote="", row.names=NULL)
joong<-read.csv("./data/지역구_매출/중구.csv", fileEncoding = "utf-8", quote="", row.names=NULL)

#변수이름 변경
enterprise<-rename(enterprise,X.기준년월=X..,X.시도명=X.기준년월.,X.시군구명=X.시도명.,X.업종중분류명=X.업종대분류명.,X.영업이익총액=X.매출중위액.)

# 서울시, 그리고 기준년월/시군구명, 영업이익총액, 업종중분류명 추출
enterprise<-enterprise %>% filter(X.시도명 == "\"서울\"") %>%
  select(X.기준년월,X.시군구명,X.영업이익총액,X.업종중분류명)

#결측치체크
table(is.na(enterprise$X.영업이익총액))

#기준년월, 매출총액, 영업이익총액, 지역구 중 결측치 있는 행 전부 제거
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]

#데이터 추출
joong2018<-enterprise %>% filter(X.시군구명 =="\"중구\""&X.기준년월=="\"2018-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

joong2019<-enterprise %>% filter(X.시군구명 =="\"중구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

joong2020<-enterprise %>% filter(X.시군구명 =="\"중구\""&X.기준년월=="\"2020-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

View(joong)
joong<-joong[,-c(1)]
# 한 번 데이터 보기
joong_ts<-ts(joong)
plot.ts(joong_ts)

auto.arima(joong_ts)
