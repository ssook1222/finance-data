# 라이브러리 추가
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("extrafont")
library(extrafont)
font_import()
y

# 파일 불러오기
mobil_after<-read.csv("./apple_mobility.csv", fileEncoding = "utf-8", quote="")
mobil_after

#변수이름 변경

