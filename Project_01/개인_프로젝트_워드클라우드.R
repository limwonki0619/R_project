# 워드클라우드 

### 엑셀 불러오기 --------------------------------------------------------------------------------------------------------------
setwd("D:/limworkspace/R_project/Project_01")

library(xlsx)
library(dplyr)

OG_data <- read.xlsx2("네이버_영화_리뷰_알라딘.xlsx", sheetIndex=1, header=T, stringsAsFactors=F)
dataset <- OG_data # 원본데이터 복사 (엑셀파일 불러올 때 메모리오류로 인해 에러가 자주남) # options(java.parameters = "-Xmx7168m")
str(dataset)

# 데이터 형태 맞춰주기 ---------------------------------
dataset$점수 <- as.numeric(dataset$점수)
dataset$공감 <- as.numeric(dataset$공감)
dataset$비공감 <- as.numeric(dataset$비공감)
dataset$요일 <- factor(dataset$요일, levels = c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"),
                     labels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일"))

str(dataset)

# 1. 워드클라우드 만들기 -------------------------------
# 필요패키지 설치 --------------------------------------
install.packages("rJava")
library(rJava) 
library(KoNLP)
library(dplyr)
library(stringr)
library(wordcloud2)

install.packages("tidyverse")
library(tidyverse)
install.packages("reshape2")
library(reshape2)
library(Rcpp)

useNIADic()    # KoNLP 패키지에 있는 형태소사전(NIADic) 사용 선언
useSejongDic() # 세종사전 호출 

# 방법 1 : extractNoun
# 데이터 정제 --------------------------------------
write.table(dataset$리뷰,"알라딘_리뷰.txt", row.names=F, col.names=F) # 텍스트파일로 만들기 생략 

text <- as.character(dataset$리뷰)
text2 <- sapply(text, extractNoun, USE.NAMES = F) 
text3 <- unlist(text2) %>% str_match("([가-힣]+)")
text4 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 7},text3)

write.table(text4,"알라딘_리뷰_전처리_후.txt", row.names=F, col.names=F, quote=F) # 임시저장
text4 <- readLines("알라딘_리뷰_전처리_후.txt")

head(sort(table(text4), decreasing = T),50)
head(sort(table(text5), decreasing = T),50)

# 데이터 전처리 ----------------------------------------
text5 <- gsub("재밌어요","재미", text4)
text5 <- gsub("재밌고","재미", text5)
text5 <- gsub("재밌게","재미", text5)
text5 <- gsub("쟈스민","자스민", text5)
text5 <- gsub("지니가","지니", text5)
text5 <- gsub("윌스미스가","윌스미스", text5)
text5 <- gsub("월스미스","윌스미스", text5)
text5 <- gsub("관람객너무","관람객", text5)

gsub_txt <- readLines("gsubfile.txt")
for (i in 1:length(gsub_txt)) {
  text5  <- gsub((gsub_txt[i]),"",text5)
}

text5 <- text5[grep("[가-힣]+",text5)]

# 워드클라우드
wordcount <- head(sort(table(text5), decreasing = T),50)
wordcount

wordcloud2(wordcount, size=0.8, 
           col="random-light", backgroundColor="black",
           fontFamily='나눔바른고딕')
