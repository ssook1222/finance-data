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
enterprise<-rename(enterprise,X.기준년월=X..,X.시도명=X.기준년월.,X.시군구명=X.시도명.,X.업종중분류명=X.업종대분류명.,X.영업이익총액=X.매출중위액.)

# 서울시, 그리고 기준년월/시군구명, 영업이익총액, 업종중분류명 추출
enterprise<-enterprise %>% filter(X.시도명 == "\"서울\"") %>%
  select(X.기준년월,X.시군구명,X.영업이익총액,X.업종중분류명)

#기준년월, 매출총액, 영업이익총액, 지역구 중 결측치 있는 행 전부 제거
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]

# 영업이익총액 이상치 제거(상하위 970개)
enterprise$X.영업이익총액 <- ifelse(enterprise$X.영업이익총액 < -5810949 | enterprise$X.영업이익총액 > 129031859, NA, enterprise$X.영업이익총액)
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]

locate<-enterprise %>% filter(X.시군구명 == "\"영등포구\""&X.기준년월=="\"2020-12\"") %>%
  # 각 분류별로 묶기
  select(X.영업이익총액,X.업종중분류명)

# 막대그래프로 표현
ggplot(locate,aes(x=X.업종중분류명,y=X.영업이익총액))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=4.5, family = "NanumGothic"))

ggsave("영등포.jpg", width=35,height=10,dpi = 600)
