#라이브러리 추가
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("extrafont")
library(extrafont)
font_import()
y

#데이터 추가
customer<-read.csv("../financialData/sinhan.csv", fileEncoding = "euc-kr", quote="")
customer

#결측치 개수 빈도별 측정
table(is.na(customer$가맹점매출))
table(is.na(customer$지역구))
table(is.na(customer$기준년월))
table(is.na(customer$직장인여부))
table(is.na(customer$총지출))

#총지출내역과 가맹점매출 변수추출
customer<-customer %>% filter(총지출!=0&가맹점매출!=0) %>%
  select(지역구, 기준년월, 가맹점매출, 직장인여부, 총지출)

#가맹점 매출, 총 지출 요약해서 보여주기
summary(customer$가맹점매출)
summary(customer$총지출)

# 이상치 제거 전 가맹점 매출, 총 지출 box plot
boxplot(customer$가맹점매출)
boxplot(customer$총지출)

# 이상치 있는 열 확인
# 가맹점 매출 상한 이상치 개수
which(customer$가맹점매출>summary(customer$가맹점매출)[5] + 1.5*IQR(customer$가맹점매출))

# 가맹점 매출 하한 이상치 개수 = 없음
which(customer$가맹점매출<summary(customer$가맹점매출)[2] - 1.5*IQR(customer$가맹점매출))

# 총지출 상한 이상치 개수 
which(customer$총지출>summary(customer$총지출)[5] + 1.5*IQR(customer$총지출))

# 총지출 하한 이상치 개수 = 없음
which(customer$총지출<summary(customer$총지출)[2] - 1.5*IQR(customer$총지출))

# 총지출 이상치 제거
customer <- customer[-which(customer$총지출>summary(customer$총지출)[5] + 1.5*IQR(customer$총지출)),]
boxplot(customer$총지출)
boxplot(customer$총지출)$stats

# 총지출 이상치 제거 
customer <- customer[-which(customer$가맹점매출>summary(customer$가맹점매출)[5] + 1.5*IQR(customer$총지출)),]
boxplot(customer$가맹점매출)
boxplot(customer$가맹점매출)$stats

summary(customer$가맹점매출)
summary(customer$총지출)

str(customer)
# 가맹점 매출_지역구 기준년월 그룹별로 묶기
customer %>%
  group_by(지역구, 기준년월) %>%
      summarise(avg=mean(가맹점매출),med=median(가맹점매출))
  view()
  
# 총지출_지역구 기준년월 그룹별로 묶기
  customer %>%
    group_by(지역구, 기준년월) %>%
    summarise(avg=mean(총지출),med=median(총지출))
  view()

# 막대그래프를 그려보기   
  ggplot(customer,aes(x=지역구,y=가맹점매출,fill=기준년월))+
    geom_bar(stat="identity")+
    theme(text=element_text(size=5, family = "NanumGothic"))
  
  ggplot(customer,aes(x=지역구,y=총지출,fill=기준년월))+
    geom_bar(stat="identity")+
    theme(text=element_text(size=5, family = "NanumGothic"))
