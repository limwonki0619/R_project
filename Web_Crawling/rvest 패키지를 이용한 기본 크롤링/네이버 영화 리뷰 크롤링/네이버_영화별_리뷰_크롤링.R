setwd("D:/limworkspace/R_project/Web_Crawling/네이버 영화 리뷰 크롤링")

library(rvest)
library(dplyr)
library(stringr)

trim <- function(text){gsub("^\\s+|\\s+$","",text)}

# ------------------------------------------------------------------------------

base_url_1 <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code="
code <- c("173123","163788") # 영화코드 순서대로 (스파이더맨, 알라딘)
codename <- c("스파이더맨","알라딘")
type <- "after"  # 영화관람 후
base_url_2 <- "&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
page <- 1:2

main_url <- NULL
movie_page_url <- NULL
reviews <- NULL
lis <- NULL 
score <- NULL
user_name <- NULL 
date <- NULL
updown <- NULL
code_num <- NULL
code_name <- NULL 

for (i in 1:2) { # 영화별 리뷰 url 생성
  main_url[i] <- paste0(base_url_1, code[i])
  movie_page_url <- c(movie_page_url, paste0(main_url[i],"&type=", type, base_url_2, 1:2))
  
  for (url in movie_page_url) {
    html <- read_html(url)
    lis <- html %>% html_node("div.score_result") %>% html_nodes("li") 
    reviews <- c(reviews, lis %>% html_node(".score_reple") %>% html_node("p") %>% html_text())
    score <- c(score, lis %>% html_node(".star_score") %>% html_text() %>% trim())
    user_name <- c(user_name, lis %>% html_node(".score_reple") %>% html_node("em") %>% html_text() %>% trim())
    date <- c(date, lis %>% html_node(".score_reple") %>% html_nodes("dt") %>% html_text() %>% trim() %>% str_sub(-16, -1))
    updown <- c(updown, lis %>% html_node(".btn_area") %>% html_text() %>% trim() %>% str_remove_all("\t|\n|공감|비공감"))
    code_num <- c(code_num, rep(code[i], length(lis)) )
    code_name <- c(code_name, rep(codename[i], length(lis)))
  }
  
}

naver_movie_reviews <- data.frame(영화이름=code_name,
                                  영화코드=code_num,
                                  이름=user_name,
                                  리뷰=reviews,
                                  점수=score,
                                  공감=str_split_fixed(updown,"\r",2)[,1],
                                  비공감=str_split_fixed(updown,"\r",2)[,2],
                                  입력날짜=str_sub(date,1,10),
                                  입력시간=str_sub(date,-5,-1))

library(xlsx)
write.xlsx(naver_movie_reviews, file="네이버_영화별_리뷰.xlsx",row.names=F)
