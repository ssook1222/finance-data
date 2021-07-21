# 데이터 병합하기
setwd("./Data")  
list.files() ## 해당 파일에 있는 리스트 가져오기

file_list<-list.files() ## 가져온 리스트 변수로 저장 
file_list

final<-NULL ## 저장할 데이터 프레임 빈 변수 (NULL)로 선언 
for(i in 1:length(file_list)){ ## for문으로 읽어오기  
  file<-read.csv(file_list[i]) 
  final<-rbind(final,file)  ## 묶어서 final에 넣기
  ## cat("\n",i) 제대로 되고 있는지 출력용
}
# 병합한 데이터 ent.csv로 저장 
write.csv(final,file="ent.csv")

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
enterprise<-read.csv("../ent.csv", fileEncoding = "utf-8", quote="", row.names=NULL)

#변수이름 변경
enterprise<-rename(enterprise,X.기준년월=X..,X.시도명=X.기준년월.,X.시군구명=X.시도명.,X.업종중분류명=X.업종대분류명.,X.영업이익총액=X.매출중위액.)

# 서울시, 그리고 기준년월/시군구명, 영업이익총액, 업종중분류명 추출
enterprise<-enterprise %>% filter(X.시도명 == "\"서울\"") %>%
  select(X.기준년월,X.시군구명,X.영업이익총액,X.업종중분류명)

#결측치체크
table(is.na(enterprise$X.영업이익총액))

#기준년월, 매출총액, 영업이익총액, 지역구 중 결측치 있는 행 전부 제거
enterprise <- enterprise[ complete.cases(enterprise[ , c("X.영업이익총액")]), ]

# 영업이익총액 요약해서 보여주기
summary(enterprise$X.영업이익총액)

# 이상치 제거 전 영입이익총액  box plot
boxplot(enterprise$X.영업이익총액)

# 영업이익총액 상한 이상치 개수 
which(enterprise$X.영업이익총액>summary(enterprise$X.영업이익총액)[5] + 1.5*IQR(enterprise$X.영업이익총액))

# 영업이익총액 하한 이상치 개수 = 없음
which(enterprise$X.영업이익총액<summary(enterprise$X.영업이익총액)[2] - 1.5*IQR(enterprise$X.영업이익총액))

# 영업이익총액 이상치 제거
enterprise <- enterprise[-which(enterprise$X.영업이익총액>summary(enterprise$X.영업이익총액)[5] + 1.5*IQR(enterprise$X.영업이익총액)),]
enterprise <- enterprise[-which(enterprise$X.영업이익총액>summary(enterprise$X.영업이익총액)[2] - 1.5*IQR(enterprise$X.영업이익총액)),]
boxplot(enterprise$X.영업이익총액)
boxplot(enterprise$X.영업이익총액)$stats

# 영업이익총액_지역구 기준년월 그룹별로 묶기
enterprise %>%
  group_by(X.시군구명, X.기준년월) %>%
  summarise(avg=mean(X.영업이익총액),med=median(X.영업이익총액))
view()

enterprise %>%
  group_by(X.시군구명, X.업종중분류명) %>%
  summarise(avg=mean(X.영업이익총액),med=median(X.영업이익총액))
view()

#막대그래프로 표현
ggplot(enterprise,aes(x=X.시군구명,y=X.영업이익총액,fill=X.기준년월))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=5, family = "NanumGothic"))

ggplot(enterprise,aes(x=X.업종중분류명,y=X.영업이익총액,fill=X.기준년월))+
  geom_bar(stat="identity")+
  theme(text=element_text(size=5, family = "NanumGothic"))

ggsave("eik.jpg", width=35,height=10,dpi = 600)



