setwd("D:/limworkspace/R_project/Web_Crawling/한빛아카데미 크롤링")

# R 크롤링 관련 패키지 및 함수 -------------------------------------------

# 1. 요청     : curl, httr, rvest, Rselenium
# 2. 정리     : 정규표현식, jsonlite, rvest, stringr
# 3. 저장     : write.()
# 4. 반복     : for, parallel
# 5. 예외처리 : try, if
# 6. 최적화   : profvis, microbenchmark

# CSS 선택자가 동작하는 방식 
#   1. tag 이름
#   2. tag의 id 속성
#   3. tag의 class 속성
#   4. tag의 custom 속성

# 한빛아카데미 도서 크롤링 ----------------------------------------------- 

install.packages("rvest")
library(rvest)
library(dplyr)
library(stringr)

trim <- function(x){gsub("^\\s+|\\s+$","",x)} # 정규표현식을 이용한 white space 제거 
base_url <- "http://www.hanbit.co.kr/academy/books/new_book_list.html"
html <- read_html(base_url)
book_list <- html_node(html, ".sub_book_list_area") # node는 한개, class="sub_book_list_area" 검색 
lis <- html_nodes(book_list, "li") # nodes는 다수, <li> 검색 

# dplyr을 이용한 크롤링 : 하나의 페이지 크롤링 
lis <- html %>% html_node(".sub_book_list_area") %>% html_nodes("li") 


price <- NULL
title <- NULL 
writer <- NULL

for (li in lis) {
  price <- c(price, html_nodes(li, ".price") %>% html_text())
  title <- c(title, html_nodes(li, ".book_tit") %>% html_text())
  writer <-c(writer, html_nodes(li,".book_writer") %>% html_text())
  # cat(title, writer, price, "\n")
}

books <- data.frame(price = price,
                    title = title,
                    writer = writer)


# dplyr을 이용한 크롤링 : 여러 페이지 크롤링 
base_url <- "http://www.hanbit.co.kr/academy/books/new_book_list.html?page="
categori <- "&cate_cd=&srt=&searchKey=&keyWord="

urls <- NULL
for (i in 1:10) {
  urls[i] <- paste0(base_url,i,categori)
}

price <- NULL
title <- NULL
writer <- NULL 

for (url in urls) {
  html <- read_html(url)
  price <- c(price, html %>% html_node(".sub_book_list_area") %>% html_nodes("li") %>% html_nodes(".price") %>% html_text())
  title <- c(title, html %>% html_node(".sub_book_list_area") %>% html_nodes("li") %>% html_nodes(".book_tit") %>% html_text())
  writer <- c(writer, html %>% html_node(".sub_book_list_area") %>% html_nodes("li") %>% html_nodes(".book_writer") %>% html_text())
}

computer_books <- data.frame(
  title = title,
  writer = writer,
  price = price
)




# *** 카테고리별 모든 페이지 크롤링 -------------------------------------------------------------------------------------------------------------

base_url <- "http://www.hanbit.co.kr/academy/books/category_list.html?"
categori <- "cate_cd="
categori_num <- c("004003", "004004", "004005", "004006", "004007", "004008", "005001", "005002", "005003", "005004", "005005")

urls <- NULL
for (i in categori_num) {
  urls <- c(urls,paste0(base_url,categori, i))  
}

page <- NULL # 카테고리별 페이지 
for (i in urls) {
  page <- c(page, read_html(i) %>% html_node(".paginate") %>% html_nodes("a") %>% html_attr("href") %>% length()+1)
}
sum(page) # 총페이지 갯수 

# 모든 url 생성 ----------------------------------------------------------------------------------------------------------------

all_urls <- NULL
k <- 0
for (cn in categori_num) {
  k <- k+length(i)          # 카테고리별 페이지 수를 만들기 위한 상수
  for (i in 1:page[k]) {    # 카테고리별 페이지 수 생성 
    all_urls <- c(all_urls, paste0(base_url,"page=",i,"&",categori, cn))  
  }  
}

# 모든 url에서 책 정보 가져오기 ------------------------------------------------------------------------------------------------

price <- NULL # for문 돌리기 전, 빈 변수 생성 필수 
title <- NULL
writer <- NULL
categori_name <- NULL

for (total in all_urls) {
  html <- read_html(total)
  lis <- html %>% html_node(".sub_book_list_area") %>% html_nodes("li")   # 책 정보가 있는 공용 태그
  
  price <- c(price, lis %>% html_nodes(".price") %>% html_text())         # 책 정보 중 가격 가져오기
  title <- c(title, lis %>% html_nodes(".book_tit") %>% html_text())      # 책 정보 중 제목 가져오기
  writer <- c(writer, lis %>% html_nodes(".book_writer") %>% html_text()) # 책 정보 중 작가이름 가져오기
  categori_name <- c(categori_name, html %>% html_nodes(".docu_title") %>% html_text() %>% rep(length(lis))) # 카테고리를 lis갯수만큼 반복 (다른 class에 위치해 있기때문에 임의로 생성)
}

trim <- function(x) gsub("^\\s+|\\s+$", "", x) # 정규식을 이용한 각종 html 찌꺼기 <\r><\n> 등등 제거 

all_page_categori <- data.frame(가격 = paste0(gsub("\\\\","",price),"원"), # '\\'삭제 후 '원'추가 
                                책제목 = title,
                                작가이름 = writer,
                                카테고리 = trim(categori_name)) # 불필요 문자 삭제 
View(all_page_categori)
write.csv(all_page_categori, file="한빛아카데미_카테고리별_모든페이지_크롤링.csv", row.names=F)

