setwd("D:/limworkspace/R_project/Project_01")

library(rvest)
library(dplyr)
library(stringr)

trim <- function(text){gsub("\\s+","",text)} # str_trim(text, c("both"))와 같음 

# 기본 url 생성 -------------------------------------------------------------------------
base_url_1 <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=" # 바뀌지 않는 앞쪽 url
moviecode <- "163788"     # 영화코드 
moviename <- "알라딘" # 영화이름
type <- "after"           # 영화관람 후
base_url_2 <- "&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=" # page를 포함한 뒤쪽 url

# 영화 코드별 메인 url ------------------------------------------------------------------
main_url <- paste0(base_url_1, moviecode) # 필수 url

# 총 페이지 수 --------------------------------------------------------------------------
total_page_num <- paste0(main_url, "&type=", type, base_url_2, "1") %>% 
  read_html() %>% 
  html_node(".score_total") %>% 
  html_node(".total") %>% 
  html_nodes("em") %>% .[2] %>% html_text() %>% str_remove(",") %>% as.numeric()/10

# page url 생성 -------------------------------------------------------------------------
page_url <- NULL

for (page in 1:ceiling(total_page_num)) { # 총 페이지 수를 올림 
  page_url[page] <- paste0(main_url, "&type=", type, base_url_2, page)
}

# 정보 출력 loop ------------------------------------------------------------------------
reviews <- NULL
lis <- NULL 
score <- NULL
user_name <- NULL 
date <- NULL
up <- NULL
down <- NULL
movie_code <- NULL
movie_name <- NULL
url_num <- 0

for (url in page_url) {
  url_num <- url_num+length(url)
  if(url_num%%10==0){ 
    print(url_num)} # 출력중인 url 보이기 (10단위) 
  
  html <- read_html(url)
  lis <- html %>% html_node("div.score_result") %>% html_nodes("li") # 공통 경로
  
  reviews <- c(reviews, lis %>% # 리뷰
                 html_node(".score_reple") %>% 
                 html_node("p") %>% 
                 html_text()) 
  
  user_name <- c(user_name, lis %>% # 작성자 닉네임 
                   html_node(".score_reple") %>% 
                   html_nodes("em") %>% 
                   .[seq(1, length(lis)*3, by=3)] %>%
                   html_text() %>% trim()) 
  
  date <-  c(date, lis %>% # 작성날짜 및 시간 
               html_node(".score_reple") %>% 
               html_nodes("em")  %>%  
               .[seq(2, length(lis)*3, by=3)] %>%
               html_text() %>% 
               str_sub(., 1, 10))  # factor를 Date형식으로 변환 후 저장 
  
  score <- c(score, lis %>% # 평점
               html_node(".star_score") %>% 
               html_text() %>% 
               trim()) 
  
  up <- c(up, lis %>% # 공감 수
            html_node(".btn_area") %>% 
            html_nodes("strong") %>% 
            .[grep("sympathy",.)] %>% 
            html_text())         
  
  down <- c(down, lis %>% # 비공감 수
              html_node(".btn_area") %>% 
              html_nodes("strong") %>% 
              .[grep("notSympathy",.)] %>% 
              html_text()) 
  
  movie_code <- c(movie_code, rep(moviecode, length(lis))) # 영화코드
  movie_name <- c(movie_name, rep(moviename, length(lis))) # 영화이름
}

naver_movie_reviews <- data.frame(영화명=movie_name,
                                     영화코드=movie_code,
                                     이름=user_name,
                                     리뷰=reviews,
                                     점수=score,
                                     공감=up,
                                     비공감=down,
                                     날짜=as.Date(date,"%Y.%m.%d"),           # %Y.%m.%d으로 기존 날짜 형식의 형태를 알려줌 
                                     요일=weekdays(as.Date(date,"%Y.%m.%d"))) # weekdays 함수로 날짜에 맞는 요일 출력 
library(xlsx)
write.xlsx(naver_movie_reviews, file=paste0("네이버_영화_리뷰_",moviename,".xlsx"), row.names=F) # write.xlsx(변수, 파일명, ...)
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




