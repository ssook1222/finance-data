#패키지 설치 및 라이브러리 추가
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("UsingR")
library(UsingR)
install.packages("readxl")
library(readxl)

#데이터 불러오기
shin <- readxl::read_excel(path="C:/data_fi/금융대회_신한은행.xlsx",
                        sheet = "finance_data",
                        col_names=TRUE)

dim(shin)
dim(shin[complete.cases(shin),])
head(shin)

#중복값확인
duplicates <- s %>% duplicated() %>% table()
duplicates

#결측값확인
table(is.na(s$가맹점매출입금))
table(is.na(s$지역구))
table(is.na(s$기준년월))
table(is.na(s$신용대출금액))
table(is.na(s$담보대출금액))


#변수 생성 및 필요한 데이터 추출
shin <- shin %>%
  mutate(대출총합 = 신용대출금액+담보대출금액)

shin <- shin %>% filter(가맹점매출입금!=0&대출총합!=0) %>% dplyr::select(기준년월, 지역구, 가맹점매출입금, 대출총합)
head(shin)
summary(shin)

#데이터 요약 및 상자그림
summary(card$대출총합)
summary(card$가맹점매출입금)
boxplot(card$대출총합)
boxplot(card$가맹점매출입금)

#aggregate함수 사용
card <- aggregate(대출총합~기준년월+가맹점매출입금, shin, mean)

#기준년월로 나누기
card_201903 <- card %>% filter(기준년월==201903)
card_201909 <- card %>% filter(기준년월==201909)
card_202003 <- card %>% filter(기준년월==202003)
card_202009 <- card %>% filter(기준년월==202009)
card_202103 <- card %>% filter(기준년월==202103)

