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

enterprise$X.기준년월

#각 지역구별 변수 생성
중구<-enterprise %>% filter(X.시군구명 =="\"중구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

종로구<-enterprise %>% filter(X.시군구명 =="\"종로구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

강남구<-enterprise %>% filter(X.시군구명 =="\"강남구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

강서구<-enterprise %>% filter(X.시군구명 =="\"강서구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

강동구<-enterprise %>% filter(X.시군구명 =="\"강동구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

강북구<-enterprise %>% filter(X.시군구명 =="\"강북구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

성북구<-enterprise %>% filter(X.시군구명 =="\"성북구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

노원구<-enterprise %>% filter(X.시군구명 =="\"노원구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

양천구<-enterprise %>% filter(X.시군구명 =="\"양천구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

마포구<-enterprise %>% filter(X.시군구명 =="\"마포구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

성동구<-enterprise %>% filter(X.시군구명 =="\"성동구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

광진구<-enterprise %>% filter(X.시군구명 =="\"광진구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

동작구<-enterprise %>% filter(X.시군구명 =="\"동작구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

도봉구<-enterprise %>% filter(X.시군구명 =="\"도봉구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

서대문구<-enterprise %>% filter(X.시군구명 =="\"서대문구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

송파구<-enterprise %>% filter(X.시군구명 =="\"송파구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

영등포구<-enterprise %>% filter(X.시군구명 =="\"영등포구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

은평구<-enterprise %>% filter(X.시군구명 =="\"은평구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

서초구<-enterprise %>% filter(X.시군구명 =="\"서초구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

중랑구<-enterprise %>% filter(X.시군구명 =="\"중랑구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

동대문구<-enterprise %>% filter(X.시군구명 =="\"동대문구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액))

용산구<-enterprise %>% filter(X.시군구명 =="\"용산구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

관악구<-enterprise %>% filter(X.시군구명 =="\"관악구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

구로구<-enterprise %>% filter(X.시군구명 =="\"구로구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))

금천구<-enterprise %>% filter(X.시군구명 =="\"금천구\""&X.기준년월=="\"2019-12\"") %>%
  select(X.영업이익총액,X.업종중분류명) %>%
  group_by(X.업종중분류명) %>%
  summarise(sum=sum(X.영업이익총액),min=min(X.영업이익총액))
