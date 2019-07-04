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




