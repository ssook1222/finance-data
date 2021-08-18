
names(agg_sub)[3] <- c("총계평균")
head(agg_sub)

#구별 나누기
sub_gangseo<-agg_sub %>% filter(지역구=='강서구')
sub_yang<-agg_sub %>% filter(지역구=='양천구')
sub_keum<-agg_sub %>% filter(지역구=='금천구')
sub_kwan<-agg_sub %>% filter(지역구=='관악구')
sub_eun<-agg_sub %>% filter(지역구=='은평구')
sub_guro<-agg_sub %>% filter(지역구=='구로구')
sub_dobong<-agg_sub %>% filter(지역구=='도봉구')
sub_jlang<-agg_sub %>% filter(지역구=='중랑구')
sub_gangdong<-agg_sub %>% filter(지역구=='강동구')
sub_yong<-agg_sub %>% filter(지역구=='용산구')
sub_kwang<-agg_sub %>% filter(지역구=='광진구')
sub_moon<-agg_sub %>% filter(지역구=='동대문구')
sub_noone<-agg_sub %>% filter(지역구=='노원구')
sub_kbook<-agg_sub %>% filter(지역구=='강북구')
sub_seo<-agg_sub %>% filter(지역구=='서대문구')
sub_seong<-agg_sub %>% filter(지역구=='성북구')
sub_jong<-agg_sub %>% filter(지역구=='종로구')
sub_jung<-agg_sub %>% filter(지역구=='중구')
sub_mapo<-agg_sub %>% filter(지역구=='마포구')
sub_yung<-agg_sub %>% filter(지역구=='영등포구')
sub_song<-agg_sub %>% filter(지역구=='송파구')
sub_gnam<-agg_sub %>% filter(지역구=='강남구')
sub_scho<-agg_sub %>% filter(지역구=='서초구')
sub_sdong<-agg_sub %>% filter(지역구=='성동구')
sub_djak<-agg_sub %>% filter(지역구=='동작구')


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


#필요한 데이터 추출
shin <- shin %>% filter(가맹점매출입금!=0) %>% dplyr::select(기준년월, 지역구, 가맹점매출입금)
head(shin)
summary(shin)

#기준년월로 나누기
shin_강남구 <- shin %>% filter(지역구=='강남구')
shin_강동구 <- shin %>% filter(지역구=='강동구')
shin_강북구 <- shin %>% filter(지역구=='강북구')
shin_강서구 <- shin %>% filter(지역구=='강서구')
shin_관악구 <- shin %>% filter(지역구=='관악구')
shin_광진구 <- shin %>% filter(지역구=='광진구')
shin_구로구 <- shin %>% filter(지역구=='구로구')
shin_금천구 <- shin %>% filter(지역구=='금천구')
shin_노원구 <- shin %>% filter(지역구=='노원구')
shin_도봉구 <- shin %>% filter(지역구=='도봉구')
shin_동대문구 <- shin %>% filter(지역구=='동대문구')
shin_동작구 <- shin %>% filter(지역구=='동작구')
shin_마포구 <- shin %>% filter(지역구=='마포구')
shin_서대문구 <- shin %>% filter(지역구=='서대문구')
shin_서초구 <- shin %>% filter(지역구=='서초구')
shin_성동구 <- shin %>% filter(지역구=='성동구')
shin_성북구 <- shin %>% filter(지역구=='성북구')
shin_송파구 <- shin %>% filter(지역구=='송파구')
shin_양천구 <- shin %>% filter(지역구=='양천구')
shin_영등포구 <- shin %>% filter(지역구=='영등포구')
shin_용산구 <- shin %>% filter(지역구=='용산구')
shin_은평구 <- shin %>% filter(지역구=='은평구')
shin_종로구 <- shin %>% filter(지역구=='종로구')
shin_중구 <- shin %>% filter(지역구=='중구')
shin_중랑구 <- shin %>% filter(지역구=='중랑구')



shin_강남구 <- ddply(shin_강남구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_강동구 <- ddply(shin_강동구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_강북구 <- ddply(shin_강북구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_강서구 <- ddply(shin_강서구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_관악구 <- ddply(shin_관악구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금))
shin_광진구 <- ddply(shin_광진구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_구로구 <- ddply(shin_구로구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_금천구 <- ddply(shin_금천구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_노원구 <- ddply(shin_노원구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_도봉구 <- ddply(shin_도봉구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_동대문구 <- ddply(shin_동대문구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_동작구 <- ddply(shin_동작구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_마포구 <- ddply(shin_마포구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_서대문구 <- ddply(shin_서대문구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_서초구 <- ddply(shin_서초구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_성동구 <- ddply(shin_성동구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_성북구 <- ddply(shin_성북구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_송파구 <- ddply(shin_송파구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_양천구 <- ddply(shin_양천구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_영등포구 <- ddply(shin_영등포구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_용산구 <- ddply(shin_용산구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_은평구 <- ddply(shin_은평구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_종로구 <- ddply(shin_종로구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_중구 <- ddply(shin_중구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 
shin_중랑구 <- ddply(shin_중랑구, .(기준년월), summarise, avg매출입금 = mean(가맹점매출입금)) 

