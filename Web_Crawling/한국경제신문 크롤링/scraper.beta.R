getwd()
setwd("D:/limworkspace/R_selfstudy")
getwd()
install.packages("stringr")
library(stringr)
install.packages("rJava")
library(rJava)
install.packages("rvest")  
library(rvest)
install.packages("KoNLP")
library(KoNLP)
install.packages("dplyr")
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud")
library(wordcloud)
useSejongDic()
library(data.table)


#---------------------------------chapter1 url parsing

hk_url <- "http://search.hankyung.com/apps.frm/search.news?query=%EC%A0%84%EA%B8%B0%EC%9E%90%EB%8F%99%EC%B0%A8&page=" # 쿼리지정 가능 

hk_urls <- NULL
for (x in 1:5){
  hk_urls[x] <- paste(hk_url, as.character(x),sep = "",encoding="utf-8")
}

urls <- NULL
for(url in hk_urls){
  html <- read_html(url)
  urls <- c(urls, html %>% html_nodes(".article") %>% html_nodes(".txt_wrap") %>% html_nodes("a") %>% html_attr("href")%>% unique())
}

urls <- urls[str_detect(urls, "article")] # 특정문자가 들어있는 행 찾기
# urls <- urls[-grep("tag",urls)] # 특정문자가 들어있는 행 지우기 

html2 <- NULL
links <- NULL
txts <- NULL

for(links in urls){
  html2 <- read_html(links, encoding = 'utf-8')
  txts <-  c(txts, html2 %>% html_nodes(".wrap_article") %>% html_text()) # class는 . id는 #
}

news <- cbind("검색어"="전기자동차",url=urls,content=unlist(txts))
news <- as.data.frame(news)
View(news)
write.csv(news,"news.csv")

#----------------------------------chapter2 data cleaning

text <- gsub("\\n","",text)
text <- gsub("\\d+","",text)
text <- gsub("\\.","",text)
text <- gsub("\r","",text)
text <- gsub("\t","",text)
text <- gsub("[A-z]","",text)
text <- gsub("[[:cntrl:]]","",text)
text2 <- sapply(text,extractNoun,USE.NAMES = F)
text3 <- unlist(text2)
text4 <- Filter(function(x){nchar(x)>=2 & nchar(x)<=5},text3)
text4 <- gsub("한경닷컴","",text4)
text4 <- gsub("무단","",text4)
text4 <- gsub("전재","",text4)
text4 <- gsub("구독","",text4)
text4 <- gsub("뉴스래빗","",text4)
text4 <- gsub("모바일한경","",text4)

text4 <- gsub("기사","",text4)
text4 <- gsub("들이","",text4)
text4 <- gsub("글방","",text4)
text4 <- gsub("하게","",text4)
text4 <- gsub("무엇을","",text4)
text4 <- gsub("지난달","",text4)
text4 <- gsub("하기","",text4)
text4 <- gsub("번째","",text4)

text5 <- data.table(text4)
wordcount <- table(text5)
head(sort(wordcount,decreasing=T),50)

write.table(wordcount,"wordfile.txt")    #text file saving

pal <- brewer.pal(9,"Set1")
wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0,min.freq=12,
          random.order=F,random.color=T,colors=pal)
