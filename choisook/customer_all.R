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

customer<-customer %>% filter(총지출!=0&가맹점매출!=0) %>%
  select(지역구, 기준년월, 가맹점매출, 직장인여부, 총지출, 신용대출, 담보대출)

customer<-customer %>%
  mutate(총대출 = 신용대출+담보대출)
  

# 총대출 기준년월 그룹별로 묶기
check<-customer %>%
  group_by(기준년월) %>%
      summarise(avg=mean(총대출),med=median(총대출))
write.csv(check,file="check2.csv")
  
# 총지출_지역구 기준년월 그룹별로 묶기
  customer %>%
    group_by(지역구, 기준년월) %>%
    summarise(avg=mean(총대출))
  view()

# 그래프 그리기
    ggplot(customer,aes(x=지역구,y=총대출,fill=기준년월))+
    geom_bar(stat="identity")+
    theme(text=element_text(size=5, family = "NanumGothic"))
