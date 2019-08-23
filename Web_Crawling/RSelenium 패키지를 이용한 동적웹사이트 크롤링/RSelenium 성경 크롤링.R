setwd("D:/limworkspace/R_project/Web_Crawling/RSelenium 패키지를 이용한 동적웹사이트 크롤링")

install.packages('RSelenium')
library(httr)
library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)

remDr <- remoteDriver(remoteServerAddr = 'localhost', 
                      port = 4445L, # 포트번호 입력 
                      browserName = "chrome") 
remDr$open() # 크롬창 실행
remDr$navigate("https://www.bskorea.or.kr/bible") # 해당 url로 찾아가기

# 장과 절 가져오기

char <- remDr$findElement("css", value="#chap")
char_num <- char$getElementText() %>% str_trim() %>% str_split(., "\n") %>% .[[1]]

sec <- remDr$findElement("css", value="#sec")
sec_num <- sec$getElementText() %>% str_trim() %>% str_split(., "\n") %>% .[[1]]

# 장과 절 페이지 넘기면서 텍스트 수집 

char <- remDr$findElement("css", value="#chap")
char$sendKeysToElement(list(char_num[6]))

button <- remDr$findElement("css", value="input.searchBtn")
button$clickElement()

Sys.sleep(1)

url = remDr$getPageSource()[[1]]
html = read_html(url, encoding="UTF-8")
text = html %>% html_nodes("#tdBible1 > span") %>% html_text()
