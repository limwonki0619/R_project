setwd("D:/limworkspace/R_project/Web_Crawling/RSelenium 패키지를 이용한 동적웹사이트 크롤링")

# 참고사이트 https://sancj.tistory.com/62
# 관련 프로그램 설치 https://github.com/limwonki0619/R_project/tree/master/Web_Crawling

# 패키지 설치 -------------------------------------------------------------------------
install.packages("RSelenium")
library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)

trim <- function(text){gsub("^\\s+|\\s+$","",text)} # 문자열 처리를 위한 함수 

# -------------------------------------------------------------------------------------
remDr <-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome") # 브라우저 호출 

remDr$open() # 크롬창 실행
remDr$navigate("https://nid.naver.com/nidlogin.login") # 해당 url로 찾아가기

# css 방식을 이용한 값 위치 지정 
txt_id <- remDr$findElement(using="css", value="#id")            # findElement == rvest::html_node 함수를 이용하여 특정 위치를 지정할 수 있다.
#txt_id <- remDr$findElement(using="id", value="id")                      # id값으로만 찾아가는 방식 class로도 사용가능  
txt_pw <- remDr$findElement(using="css", value="#pw")            # 이 때 "xpath" 방식과 "css selector" 방식이 있습니다. 
login_btn <- remDr$findElement(using="css", value=".btn_global") # 로그인버튼 위치지정 

txt_id$setElementAttribute("value", "limwk0619") # id를 입력해야하는 위치(txt_id)에 setElementAttribute 함수로 입력해준다 
txt_pw$setElementAttribute("value", "*********") # pw값도 마찬가지로 지정 후 값을 입력해준다 (단, 자기 패스워드를 써준다) 
login_btn$clickElement()

remDr$navigate("https://mail.naver.com/") # 메일 url

mail <- remDr$findElement("css", "#list_for_view") # 메일이 담긴 위치
mail_texts <- mail$getElementText() # 모든 텍스트 추출 
tmp <- str_split(mail_texts, '\n') %>% .[[1]] # \n기준으로 나눈 후, 데이터에서 첫 번째 속성값만 가져오기 


# 데이터 추출 loop ------------------------------------------------------------------------------------------------------
sender <- NULL
subject <- NULL
time <- NULL

for (i in 1:20) {
  sender <- c(sender, tmp[seq(1,length(tmp),3)])
  subject <- c(subject, tmp[seq(2,length(tmp),3)])
  time <- c(time, tmp[seq(3,length(tmp),3)])
}

df_mail <- data.frame(sender=sender,
                      subject=subject,
                      time=time)
View(df_mail)
remDr$close() # 마지막에는 항상 close 필수 