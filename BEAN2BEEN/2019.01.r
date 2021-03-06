# finance-data
금융데이터 경진대회 team repo

#install.packages("dplyr")
library(dplyr)

#19년 1월 자료
sub1901<-read.csv("C:\\Users\\Hanbeen Cho\\Desktop\\카드이용2019\\CARD_SUBWAY_MONTH_201901 (3).csv")
sub1901


#하루 총 이용객수(총계) 변수 추가하기
sub1901<- sub1901 %>% mutate(총계=승차총승객수+하차총승객수)
head(sub1901)

#사용일자, 노선명, 등록일자 삭제
sub1901<- sub1901 %>% select(역명, 승차총승객수, 하차총승객수,총계)
head(sub1901)

#강서구
gangseo<-sub1901 %>% filter(역명 %in% c('방화', '개화산', '김포공항', '송정', '마곡', '발산', '우장산', '화곡', '까치산', '염창', '등촌', '증미', '가양', '양천향교', '마곡나루', '신방화', '공항시장', '개화'))
head(gangseo)                             
gangseo<-gangseo %>% select(총계)
gangseo
totgangseo<-sum(gangseo)
totgangseo

#양천구
yang<-sub1901 %>% filter(역명 %in% c('신목동', '목동', '오목교', '신정네거리', '양천구청', '신정(은행정)'))
head(yang)                             
yang<-yang %>% select(총계)
yang
totyang<-sum(yang)
totyang

#금천구
keum<-sub1901 %>% filter(역명 %in% c('가산디지털단지', '독산', '금천구청'))
head(keum)    
keum<-keum %>% select(총계)
keum
totkeum<-sum(keum)
totkeum

#관악구
kwan<-sub1901 %>% filter(역명 %in% c('신림', '봉천', '서울대입구', '낙성대'))
head(kwan)  
kwan<-kwan %>% select(총계)
kwan
totkwan<-sum(kwan)
totkwan


#은평구
eun<-sub1901 %>% filter(역명 %in% c('독바위', '구파발', '연신내', '불광', '녹번', '역촌', '구산', '응암', '새절(신사)','증산(명지대앞)', '수색', '디지털미디어시티'))
head(eun)
eun<-eun %>% select(총계)
eun
toteun<-sum(eun)
toteun

#구로구
guro<-sub1901 %>% filter(역명 %in% c('온수(성공회대입구)', '천왕', '오류동', '개봉', '구일', '구로', '남구로', '대림(구로구청)', '구로디지털단지','신도림', '도림천'))
head(guro)  
guro<-guro %>% select(총계)
guro
totguro<-sum(guro)
totguro

#구로구
dobong<-sub1901 %>% filter(역명 %in% c('도봉산', '도봉', '방학', '창동', '녹천', '쌍문'))
head(dobong)  
dobong<-dobong %>% select(총계)
dobong
totdobong<-sum(dobong)
totdobong

#중랑구
jlang<-sub1901 %>% filter(역명 %in% c('먹골', '중화', '상봉(시외버스터미널)', '중량', '망우', '양원', '신내', '봉화산(서울의료원)', '면목', '사가정', '용마산'))
head(jlang)  
jlang<-jlang %>% select(총계)
jlang
totjlang<-sum(jlang)
totjlang


#강동구
gangdong<-sub1901 %>% filter(역명 %in% c('강동구청', '천호(풍납토성)', '암사', '강동', '길동', '굽은다리(강동구민회관앞)', '명일', '고덕', '상일동', '강일', '둔촌동', '둔촌오류','중앙보훈병원'))
head(gangdong) 
gangdong<-gangdong %>% select(총계)
gangdong
totgangdong<-sum(gangdong)
totgangdong


#용산구
yong<-sub1901 %>% filter(역명 %in% c('서울역', '숙대입구(갈월)', '녹사평(용산구청)', '이태원', '한강진', '한남', '서빙고', '이촌(국립중앙박물관)', '용산', '남영', '효창공원앞', '삼각지','신용산'))
head(yong) 
yong<-yong %>% select(총계)
yong
totyong<-sum(yong)
totyong


#광진구
kwang<-sub1901 %>% filter(역명 %in% c('중곡', '군자(능동)', '어린이대공원', '아차산(어린이대공원후문)', '광나루(장신대)', '건대입구', '구의(광진구청)', '강변(동서울터미널)', '뚝섬유원지'))
head(kwang) 
kwang<-kwang%>% select(총계)
kwang
totkwang<-sum(kwang)
totkwang

#동대문구
moon<-sub1901 %>% filter(역명 %in% c('신설동', '용두(동대문구청)', '제기동', '청량리(서울시립대입구)', '회기', '외대앞', '신이문', '왕십리(성동구청)'))
head(moon) 
moon<-moon%>% select(총계)
moon
totmoon<-sum(moon)
totmoon

#노원구
noone<-sub1901 %>% filter(역명 %in% c('석계', '광운대', '월계', '태릉입구', '화랑대(서울여대입구)', '공릉(서울과학기술대)', '하계', '중계', '노원', '상계','당고개', '마들', '수락산'))
head(noone) 
noone<-noone%>% select(총계)
noone
totnoone<-sum(noone)
totnoone


#강북구
kbook<-sub1901 %>% filter(역명 %in% c('솔샘', '삼양사거리', '삼양', '회계', '가오리', '4.19민주묘지', '솔밭공원', '북한산우이', '수유(강북구청)', '미아','미아사거리'))
head(kbook) 
kbook<-noone%>% select(총계)
kbook
totkbook<-sum(kbook)
totkbook


#서대문구
seo<-sub1901 %>% filter(역명 %in% c('홍제', '무악재', '서대문', '충정로(경기대입구)', '신촌(경의중앙선)', '신촌', '가좌'))
head(seo)
seo<-seo%>% select(총계)
seo
totseo<-sum(seo)
totseo


#성북구
seong<-sub1901 %>% filter(역명 %in% c('북한산보국문', '정릉', '성신여대입구', '길음', '한성대입구', '보문', '안암(고대병원앞)', '고려대(종암)','월곡(동덕여대)','상월곡(한국과학기술연구원)', '돌곶이'))
head(seong)
seong<-seong%>% select(총계)
seong
totseong<-sum(seong)
totseong

#종로구
jong<-sub1901 %>% filter(역명 %in% c('독립문', '경복궁(정부서울청사)', '안국', '종로3가', '광화문', '종각', '종로5가', '동대문','동묘앞','혜화', '창신'))
head(jong)
jong<-jong%>% select(총계)
jong
totjong<-sum(jong)
totjong


#중구
jung<-sub1901 %>% filter(역명 %in% c('시청', '을지로입구', '을지로3가', '을지로4가', '동대문역사문화공원', '신당', '청구', '약수','버티고개','동대입구', '충무로', '명동', '회현(남대문시장)'))
head(jung)
jung<-jung%>% select(총계)
jung
totjung<-sum(jung)
totjung


#마포구
mapo<-sub1901 %>% filter(역명 %in% c('월드컵경기장', '마포구청', '홍대입구', '망원', '합정', '상수', '광흥창(서강)', '대흥(서강대앞)','마포','공덕', '애오개', '이대', '아현'))
head(mapo)
mapo<-mapo%>% select(총계)
mapo
totmapo<-sum(mapo)
totmapo


#영등포구
yung<-sub1901 %>% filter(역명 %in% c('국회의사당', '여의나루', '샛강', '여의도', '대방', '신길', '영등포', '신풍','영등포시장','문래', '영등포구청', '당산', '양평', '선유도'))
head(yung)
yung<-yung%>% select(총계)
yung
totyung<-sum(yung)
totyung


#송파구
song<-sub1901 %>% filter(역명 %in% c('잠실나루', '몽촌토성(평화의문)', '잠실(송파구청)', '잠실새내', '종합운동장', '삼전', '석촌고분', '석촌','송파','가락시장', '문정', '장지', '복정', '경찰병원','오금','방이','개롱','거여','마천','올림픽공원','한성백제','송파나루'))
head(song)
song<-song%>% select(총계)
song
totsong<-sum(song)
totsong


#강남구
gnam<-sub1901 %>% filter(역명 %in% c('압구정로데오', '청담', '강남구청', '학동', '논현', '압구정', '신사', '잠원','신논현','언주', '선정릉', '삼성중앙','봉은사', '삼성(무역센터)', '선릉', '역삼', '강남', '한티', '매봉', '도곡', '대치', '학여울', '대청', '일원', '수서', '대모산입구', '개포동', '구룡')) 
head(gnam)
gnam<-gnam%>% select(총계)
gnam
totgnam<-sum(gnam)
totgnam


#서초구
scho<-sub1901 %>% filter(역명 %in% c('신반포', '구반포', '반포', '고속터미널', '사평', '내방', '총신대입구','방배', '서초', '교대(법원.검찰청)', '남부터미널', '양재(서초구청)', '양재시민의숲', '청계산입구', '남태령')) 
head(scho)
scho<-scho%>% select(총계)
scho
totscho<-sum(scho)
totscho


#성동구
sdong<-sub1901 %>% filter(역명 %in% c('상왕십리', '왕십리', '마장', '신답', '용답', '뚝섬', '한양대', '상수', '서울숲', '응봉', '옥수', '행당', '신금호'))
head(sdong)
sdong<-sdong%>% select(총계)
sdong
totsdong<-sum(sdong)
totsdong


#동작구
djak<-sub1901 %>% filter(역명 %in% c('노량진', '노들', '흑석(중앙대입구)', '동작(현충원)', '보라매', '신대방삼거리', '장승배기', '상도','숭실대입구', '남성', '신대방', '사당'))
head(djak)
djak<-djak%>% select(총계)
djak
totdjak<-sum(djak)
totdjak


#데이터프레임 생성 후 막대그래프 그리기
지역구<-c("동작구","도봉구","은평구", "강동구", "강서구","강남구","구로구","중랑구","종로구","중구","강북구","금천구","관악구","광진구","마포구","동대문구","노원구","서초구","성동구","서대문구","성북구","송파구","양천구","용산구","영등포구")
합계1<-c(13457613, 5955904, 11132183, 9129035,14540699,41110705,16924004,6218901,18235985,21684979,12794274,5361126,7583622,12271017,17906486,10787314,12794274,16011421,5859144,6645157,5236739,18903534,3187871,15172966,16973012)
DF1901<-data.frame(지역구,합계1)
DF1901

#꺾은선 그래프

library(ggplot2)
ggplot(DF1901, aes(x=지역구, y=합계1, group=1))+geom_line()+theme(plot.title=element_text(size=20))
