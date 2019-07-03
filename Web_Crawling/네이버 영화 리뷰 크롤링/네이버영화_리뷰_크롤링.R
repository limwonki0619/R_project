setwd("D:/limworkspace/R_project/Web_Crawling/네이버 영화 리뷰 크롤링")

library(rvest)
library(dplyr)
library(stringr)

trim <- function(text){gsub("^\\s+|\\s+$","",text)}

# 기본 url ------------------------------------------------------------------------------

base_url_1 <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=" # 바뀌지 않는 앞쪽 url
code <- "173123"          # 영화코드 
codename <- "스파이더맨"  # 영화이름
type <- "after"           # 영화관람 후
base_url_2 <- "&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=" # page를 포함한 뒤쪽 url

# 영화 코드별 메인 url ------------------------------------------------------------------
main_url <- paste0(base_url_1,code) # 필수 url

# 총 페이지 수 --------------------------------------------------------------------------
total_page_url <- paste0(main_url, "&type=", type, base_url_2,"1") %>% read_html() %>% html_node(".score_total") %>% 
  html_node(".total") %>% html_nodes("em") 
total_page_num <- total_page_url[2] %>% html_text() %>% str_remove(",") %>% as.numeric()/10 # 해당 url에서 크롤링해야할 총 페이지 수 만들기

# page url 생성 -------------------------------------------------------------------------
page_url <- NULL
for (page in 1:ceiling(total_page_num)) { # 올림처리
  page_url[page] <- paste0(main_url, "&type=", type, base_url_2, page)
}

# 정보 출력 -----------------------------------------------------------------------------

reviews <- NULL
lis <- NULL 
score <- NULL
user_name <- NULL 
date <- NULL
updown <- NULL
up <- NULL
down <- NULL
code_num <- NULL
code_name <- NULL
ems <- NULL


for (url in page_url) {
  html <- read_html(url)
  lis <- html %>% html_node("div.score_result") %>% html_nodes("li") # 공통 경로
  reviews <- c(reviews, lis %>% html_node(".score_reple") %>% html_node("p") %>% html_text()) # 리뷰
  score <- c(score, lis %>% html_node(".star_score") %>% html_text() %>% trim()) # 평점
  
  ems <- lis %>% html_node(".score_reple") %>% html_nodes("em") %>% html_text() %>% trim() # 닉네임, 날짜 포함 태그
  user_name<- c(user_name, ems[seq(1,length(ems),by=3)]) # 유저이름 
  date <-  c(date, ems[seq(2,length(ems),by=3)]) # 작성날짜
  
  updown <- lis %>% html_node(".btn_area") %>% html_nodes("strong")         # 공감/비공감 소스
    up <- c(up, updown[grep("sympathy",updown)] %>% html_text()) # 공갑 
  down <- c(down, updown[grep("notSympathy",updown)] %>% html_text()) # 비공감
  
  code_num <- c(code_num, rep(code, length(lis))) # 영화코드
  code_name <- c(code_name, rep(codename, length(lis))) # 영화이름
}

naver_movie_reviews <- data.frame(영화명=code_name,
                                  영화코드=code_num,
                                  이름=user_name,
                                  리뷰=reviews,
                                  점수=score,
                                  공감=up,
                                  비공감=down,
                                  입력날짜=str_sub(date,1,10),
                                  입력시간=str_sub(date,-5,-1))

library(xlsx)
write.xlsx(naver_movie_reviews, file="네이버_영화_리뷰.xlsx", row.names=F) 
