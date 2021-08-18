#라이브러리 설치
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)

#데이터 불러오기
enterprise<-read.csv("../financialData/ent.csv", fileEncoding = "utf-8", quote="", row.names=NULL)
uj<-read.csv("./data/upjong.csv", fileEncoding = "utf-8", quote="", row.names=NULL)

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
enterprise_2018<-enterprise %>% filter(X.기준년월=="\"2018-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

enterprise_2019<-enterprise %>% filter(X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

enterprise_2020<-enterprise %>% filter(X.기준년월=="\"2020-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

uj_ts<-ts(uj)
View(uj)

gas<-ts(uj$X[2:4])
plot.ts(gas)
auto.arima(gas)

