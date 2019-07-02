setwd("D:/limworkspace/R_project/Web_Crawling/동아일보 크롤링")

library(rvest)
library(dplyr)

front_url <- "http://www.donga.com/news/search?p="
back_url <- "&query=%EA%B7%80%EB%86%8D&check_news=1&more=1&sorting=1&search_date=1&v1=&v2=&range=1"

# page url 생성
page_url <- NULL
for (i in 0:1) {
  page_url[i+1] <- paste0(front_url,(i*15)+1,back_url)
}

# 제목, 내용, url, 날짜 생성 
title <- NULL
txt <- NULL
url <- NULL
date <- NULL

for (urls in page_url) {
  html <- read_html(urls)
  title <- c(title, html %>% html_nodes(".searchList") %>% html_nodes(".tit") %>% html_text())
  txt   <- c(txt, html %>% html_nodes(".searchList") %>% html_nodes(".txt") %>% html_text())
  url  <- c(url, html %>% html_nodes(".searchList") %>% html_nodes(".tit") %>% html_node("a") %>% html_attr("href"))
  date <- c(date, html %>% html_nodes(".searchList") %>% html_nodes(".tit") %>% html_nodes("span") %>% html_text())
}

trim <- function(text) {gsub("^\\s+|\\s+$|\\n","", text)}

donga_article <- data.frame(기사제목 = trim(title),
                            기사내용 = trim(txt),
                            기사입력날짜 = date[-grep("귀농", date)],
                            기사url = trim(urls))
install.packages("xlsx")
library(xlsx)

write.xlsx(donga_article, file = "동아일보_귀농관련기사_크롤링.xlsx", row.names=F)
