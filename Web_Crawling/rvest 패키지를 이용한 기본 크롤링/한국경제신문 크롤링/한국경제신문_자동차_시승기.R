setwd("D:/limworkspace/R_project/Web_Crawling/한국경제신문 크롤링")

library(rvest)
library(dplyr)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x) # 정규식을 이용한 화이트 페이스 문자 제거하는 함수 


base_url <- "http://auto.hankyung.com/autoreview?page=" # 한국경제신문 자동차 시승기 
page_url <- NULL
for (i in 1:2) { # 페이지당 기사가 20-개, 테스트가 가능한 정도만 크롤링 
  page_url[i] <- paste0(base_url,i) # page별 url생성 
}

each_urls <- NULL
for (urls in page_url) { # page 내 개별 기사 url생성 
  html <- read_html(urls)
  each_urls <- c(each_urls, html %>% html_node(".inner_list") %>% html_nodes("li") %>% html_node("a") %>% html_attr("href"))
}

title <- NULL
contents <- NULL
date <- NULL

for (real_urls in each_urls) {
  html <- read_html(real_urls)
  title <- c(title, html %>% html_node(".article_top") %>% html_node(".title") %>% html_text())
  contents <- c(contents, html %>% html_node(".wrap_article") %>% html_nodes("#articletxt")  %>% html_text())
  date <- c(date, html %>% html_node(".article_top") %>% html_node(".date-published") %>% html_text())
}

hk_cars_trial_ride <- data.frame(기사url = each_urls,
                                 기사제목 = title,
                                 기사내용 = trim(contents),
                                 기사작성날짜 = date)
View(hk_cars_trial_ride)

library(xlsx)
write.xlsx(hk_cars_trial_ride,file="한국경제신문_자동차_시승기.xlsx", row.names=F)
