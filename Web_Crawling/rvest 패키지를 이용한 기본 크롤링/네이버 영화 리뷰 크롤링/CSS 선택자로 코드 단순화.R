library(rvest)
library(dplyr)
library(stringr)

trim <- function(text){gsub("\\s+","",text)} # str_trim(text, c("both"))와 같음 

# 기본 url 생성 -------------------------------------------------------------------------
base_url_1 <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=" # 바뀌지 않는 앞쪽 url
moviecode <- "173123"     # 영화코드 
moviename <- "스파이더맨" # 영화이름
type <- "after"           # 영화관람 후
base_url_2 <- "&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=" # page를 포함한 뒤쪽 url

# 영화 코드별 메인 url ------------------------------------------------------------------
main_url <- paste0(base_url_1, moviecode) # 필수 url

# 총 페이지 수 --------------------------------------------------------------------------
total_page_num <- paste0(main_url, "&type=", type, base_url_2, "1") %>% 
  read_html() %>% 
  html_nodes(".score_total .total em") %>% .[2] %>% html_text() %>% str_remove(",") %>% as.numeric()/10

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

user_name <- NULL 

for (url in page_url) {
  url_num <- url_num+length(url)
  if(url_num%%10==0){ print(url_num) } # 출력중인 url 보이기 (10단위) 
  
  html <- read_html(url)
  user_name  <- c(user_name, html %>% html_nodes(".score_result li .score_reple em") %>%
                   .[seq(1, length(html)*15, by=3)] %>%
                   html_text() %>% trim())
}

seq(1, 100, by=3)

