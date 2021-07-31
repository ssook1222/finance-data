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
enterprise<-read.csv("../financialData/ent.csv", fileEncoding = "utf-8", quote="", row.names=NULL)

#변수이름 변경
enterprise<-rename(enterprise,X.기준년월=X..,X.시도명=X.기준년월.,X.시군구명=X.시도명.,X.업종중분류명=X.업종대분류명.,
                   X.영업이익총액=X.매출중위액.,X.매출총액=X.매출평균액.)

# 서울시, 그리고 기준년월/시군구명, 영업이익총액, 업종중분류명, X.매출총액 추출
enterprise<-enterprise %>% filter(X.시도명 == "\"서울\"") %>%
  select(X.기준년월,X.시군구명,X.영업이익총액,X.업종중분류명,X.매출총액)

#결측치체크
table(is.na(enterprise$X.영업이익총액))

#기준년월, 매출총액, 영업이익총액, 지역구 중 결측치 있는 행 전부 제거
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.매출총액")]), ]

#이상치 제거
enterprise$X.영업이익총액 <- ifelse(enterprise$X.영업이익총액>enterprise$X.매출총액, NA, enterprise$X.영업이익총액)
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]

# 형 변환
enterprise$X.매출총액<-as.numeric(enterprise$X.매출총액)
View(enterprise)

# 영업이익총액 요약해서 보여주기
summary(enterprise$X.영업이익총액)
summary(enterprise$X.매출총액)

# 영업이익총액_지역구 기준년월 그룹별로 묶기
first<-enterprise %>%
  group_by(X.시군구명, X.기준년월, X.업종중분류명) %>%
  summarise(avg=mean(X.영업이익총액),med=median(X.영업이익총액))
View(first)


#막대그래프로 표현
ggplot(enterprise,aes(x=X.시군구명,y=X.영업이익총액,fill=X.기준년월))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=5, family = "NanumGothic"))

ggplot(enterprise,aes(x=X.업종중분류명,y=X.영업이익총액,fill=X.기준년월))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=5, family = "NanumGothic"))

ggplot(영등포,aes(x=X.업종중분류명,y=X.영업이익총액))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=2, family = "NanumGothic"))


ggsave("영등포.jpg", width=35,height=10,dpi = 600)





