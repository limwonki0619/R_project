## https://developers.naver.com/main/ # 네이버 개발자 계정 홈페이지 

urlStr <- "https://openapi.naver.com/v1/search/blog.xml?" # 기본 url 생성 
searchString <- "query=육아" # 쿼리생성 
searchString <- iconv(searchString, to="UTF-8") # 인코딩 
searchString <- URLencode(searchString)
searchString

etcString <- "&display=100&start=1&sort=sim"

reqUrl <- paste(urlStr, searchString, etcString, sep="")
reqUrl # 요청할 url 생성 

install.packages("httr")
library(httr)

clientid <- "f9XDzJ5V400w6XWodLHC" # 개인 api id 값
clientSecret <- "YNILPjHtRS" # 개인 apu secret 값

apiResult <- GET(reqUrl, add_headers("X-Naver-Client-Id"=clientid, 
                                     "X-Naver-Client-Secret"=clientSecret))

apiResult # Status 값이 200이어야 정상. 500 이면 시스템 에러 
str(apiResult)

apiResult$content

result <- rawToChar(apiResult$content)
result
Encoding(result) <- "UTF-8"
result

library(stringr)


refineStr <- gsub("<(\\/?)(\\w+)*([^<>]*)>", " ", result)
refineStr <- gsub("[[:punct:]]", " ", refineStr)
refineStr <- gsub("[a-z]", " ", refineStr)
refineStr <- gsub("[0-9]", " ", refineStr)
refineStr <- gsub(" +", " ", refineStr)
refineStr

library(KoNLP)
none <- sapply(refineStr, extractNoun, USE.NAMES = F) 
none2 <- unlist(none)
none3 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 7}, none2)
head(sort(table(none3), decreasing = T), 30)

none4 <- gsub("육아","", none3)
none4 <- none4[none4 != ""]

head(sort(table(none4), decreasing = T), 30)

result <- str_split(result, "<title>")



refineStr <- gsub("<(\\/?)(\\w+)*([^<>]*)>", " ", result)
refineStr <- gsub("[[:punct:]]", " ", refineStr)
refineStr <- gsub("[a-z]", " ", refineStr)
refineStr <- gsub("[0-9]", " ", refineStr)
refineStr <- gsub(" +", " ", refineStr)
refineStr
str(refineStr)


