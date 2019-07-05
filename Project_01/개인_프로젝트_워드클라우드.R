# 워드클라우드 

### 엑셀 불러오기 --------------------------------------------------------------------------------------------------------------
setwd("D:/limworkspace/R_project/Project_01")

library(xlsx)
library(dplyr)
library(lubridate)

OG_data <- read.xlsx2("네이버_영화_리뷰_알라딘.xlsx", sheetIndex=1, header=T, stringsAsFactors=F)
dataset <- OG_data # 원본데이터 복사 (엑셀파일 불러올 때 메모리오류로 인해 에러가 자주남) # options(java.parameters = "-Xmx7168m")
str(dataset)

# 데이터 형태 맞춰주기 ---------------------------------
dataset$점수 <- as.numeric(dataset$점수)
dataset$공감 <- as.numeric(dataset$공감)
dataset$비공감 <- as.numeric(dataset$비공감)
dataset$날짜 <- ymd(dataset$날짜)
dataset$시간 <- factor(dataset$시간, levels=c(0:23), order=T)
dataset$요일 <- factor(dataset$요일, levels=c("월","화","수","목","금","토","일"), order=T)
str(dataset)

# 1. 워드클라우드 만들기 -------------------------------
# 필요패키지 설치 --------------------------------------
install.packages("rJava")
library(rJava) 
library(KoNLP)
library(dplyr)
library(stringr)
library(wordcloud2)

useNIADic()    # KoNLP 패키지에 있는 형태소사전(NIADic) 사용 선언
useSejongDic() # 세종사전 호출 

# 방법 1 : extractNoun
# 데이터 정제 --------------------------------------

text <- as.character(dataset$리뷰)
text2 <- sapply(text, extractNoun, USE.NAMES = F) 
text3 <- unlist(text2) %>% str_match("([가-힣]+)")
text4 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 7},text3)

write.table(text4,"알라딘_리뷰_전처리_후.txt", row.names=F, col.names=F, quote=F) # 임시저장
text4 <- readLines("알라딘_리뷰_전처리_후.txt")

head(sort(table(text4), decreasing = T),50)

# 데이터 전처리 ----------------------------------------
text5 <- gsub("재밌어요","재미", text4)
text5 <- gsub("재밌고","재미", text5)
text5 <- gsub("재밌게","재미", text5)
text5 <- gsub("쟈스민","자스민", text5)
text5 <- gsub("지니가","지니", text5)
text5 <- gsub("윌스미스가","윌스미스", text5)
text5 <- gsub("월스미스","윌스미스", text5)
text5 <- gsub("관람객너무","관람객", text5)

gsub_txt <- readLines("gsubfile.txt")
for (i in 1:length(gsub_txt)) {
  text5  <- gsub((gsub_txt[i]),"",text5)
}

text5 <- text5[text5 != ""] 

# 워드클라우드 : 3개 집단 
wordcount <- head(sort(table(text5), decreasing = T), 30)
wordcount
wordcloud2(wordcount, size=0.7, 
           col="random-light", backgroundColor="black",
           fontFamily='나눔바른고딕')

# 날짜별 평점 비교
library(lubridate)
library(reshape2)
library(extrafont)
library(ggplot2)

windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))

date_point <- dataset %>%
  group_by(날짜, 요일) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())

# 날짜별 평점 평균
ggplot(date_point,aes(x=날짜)) +
  geom_line(aes(y=date_mean_point), color="grey20", linetype=2) +
  geom_point(aes(y=date_mean_point), color=rainbow(43), size=3, alpha=.8) + 
  geom_text(aes(y=date_mean_point+0.02, label=round(date_mean_point,2)), family="dohyeon") +
  coord_cartesian(ylim = c(9, 9.8)) +
    theme_bw(base_family = "jalnan", base_size = 15) +
    labs(y="평점", title="날짜에 따른 평점평균 변화", subtitle="2019-05-23 ~ 2019-07-04") +
    theme(legend.title = element_blank(),
            plot.title = element_text(hjust=0.5, size=20, family = "jalnan"),
            plot.subtitle = element_text(hjust=0.5, color="grey20"),
            axis.text.x = element_text(angle = 90, vjust=0.5)) +
    scale_x_date(breaks = date_point$날짜, labels=str_sub(date_point$날짜,7,10)) 
     

# 날짜별 댓글 수
ggplot(date_point, aes(x=날짜, y=댓글수)) +
  geom_col(fill="seagreen3", alpha=0.7, color="grey20") + 
  geom_text(aes(y=댓글수+30, label=댓글수), color='grey20', family="jalnan") +
    theme_bw(base_family = "jalnan", base_size = 15) +
    labs(title="날짜별 댓글 수",y="댓글 수(단위:건)", subtitle="2019-05-23 ~ 2019-07-04") +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5, color="grey20"),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          axis.text.y = element_blank()) +
    scale_x_date(breaks = date_point$날짜, labels=str_sub(date_point$날짜,7,10)) 


# 시간대별 평점
time_point <- dataset %>% # 데이터 생성 
  group_by(시간) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감))

ggplot(time_point,aes(x=시간)) +
  geom_point(aes(y=date_mean_point, color=rainbow(24), size=2)) +
  geom_segment(aes(x=시간,xend=시간,y=0,yend=date_mean_point), color="grey50") +
  coord_cartesian(ylim = c(9, 10)) +
    theme_bw(base_family = "jalnan", base_size = 15) +
    theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=20, family = "jalnan")) +
    labs(y="평점평균", title="시간대별 평점평균 변화") +
  geom_text(aes(y=date_mean_point+0.05, label=round(date_mean_point,2), family="dohyeon"))


wday_point <- dataset %>% 
  group_by(요일) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())





# 요일별 평점 : 수정필요
ggplot(date_point,aes(x=요일, fill=요일)) +
  geom_boxplot(aes(y=date_mean_point), outlier.color = "grey20") +
  theme_bw(base_family = "jalnan", base_size = 15) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust=0.5, size=20, family = "jalnan")) +
  labs(y="평점평균", title="요일별 평점평균") +
  stat_summary(aes(y=date_mean_point),fun.y="mean", geom="point", shape=1, color="Coral")

# 요일별 댓글 수
ggplot(wday_point, aes(x=요일, y=댓글수)) +
  geom_col(fill="Coral", alpha=0.7, color="grey20") + 
  geom_text(aes(y=댓글수+120, label=댓글수), color='grey20', family="jalnan") +
  theme_bw(base_family = "jalnan", base_size = 15) +
  labs(title="요일별 댓글 수",y="댓글 수(단위:건)", subtitle="2019-05-23 ~ 2019-07-04") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color="grey20"),
        axis.text.x = element_text(vjust=0.5),
        axis.text.y = element_blank()) 


