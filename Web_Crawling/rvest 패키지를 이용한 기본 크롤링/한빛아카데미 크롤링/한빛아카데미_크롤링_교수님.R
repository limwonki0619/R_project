# 한빛아카데미 도서 크롤링
library(rvest)
library(dplyr)
library(stringr)
#library(xlsx)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

cat_names = c('컴퓨터공학', '정보통신_전기_전자', '수학_과학_공학', '프로그래밍_웹',
              '그래픽_디자인', 'OA_활용', '전기기본서', '전기기사',
              '전기산업기사', '전기공사기사', '전기공사산업기사')
categories = c('004007', '004008', '004003', '004004', '004005', '004006',
               '005005', '005001', '005002', '005003', '005004')
pages = c(6, 4, 3, 3, 1, 2, 2, 2, 1, 1, 1)

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page <- 'page='
category <- '&cate_cd='
sort <- '&srt=p_pub_date'

wb <- createWorkbook()
for (cat_no in 1:11) {
  df_books <- data.frame(title=c(), writer=c(), price=c())
  print(cat_no)
  
  for (i in 1:pages[cat_no]) {
    url <- paste0(base_url, page, i, category, categories[cat_no], sort)
    html <- read_html(url)
    
    lis <- html %>% html_node('.sub_book_list_area') %>% html_nodes('li') 
    
    price <- c()
    title <- c()
    writer <- c()
    
    for (li in lis) {
      price <- c(price, html_node(li, '.price') %>% html_text())
      title <- c(title, html_node(li, '.book_tit') %>% html_text())
      writer <- c(writer, html_node(li, '.book_writer') %>% html_text())
    }
    books <- data.frame(title=title, writer=writer, price=gsub("\\\\","",price))
    df_books <- rbind.data.frame(df_books, books)
  }
  
  # filename <- paste0("D:/Workspace/R_Project/01_Crawling/books/", cat_no, ".xlsx")
  # write.xlsx(df_books, file=filename, 
  #            sheetName=cat_names[cat_no],
  #            col.names=TRUE, row.names=FALSE, append=TRUE)
  
  addWorksheet(wb, cat_names[cat_no])
  writeDataTable(wb, cat_names[cat_no], df_books)
} 
saveWorkbook(wb, file="D:/Workspace/R_Project/01_Crawling/books.xlsx")