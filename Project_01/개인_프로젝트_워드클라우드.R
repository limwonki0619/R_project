## 엑셀 불러오기 --------------------------------------------------------------------------------------------------------------
setwd("D:/limworkspace/R_project/Project_01")

# 필요패키지 설치 --------------------------------------
library(xlsx)
library(lubridate)
library(extrafont)
windowsFonts(dohyeon=windowsFont("BM DoHyeon"))
windowsFonts(jalnan=windowsFont("Jalnan"))
library(ggplot2)

# 수집된 데이터 불러오기 -------------------------------- 
OG_data <- read.xlsx2("네이버_영화_리뷰_알라딘.xlsx", sheetIndex=1, header=T, stringsAsFactors=F)
dataset <- OG_data # 원본데이터 복사 (엑셀파일 불러올 때 메모리오류로 인해 에러가 자주남) # options(java.parameters = "-Xmx7168m")
table(OG_data$점수) # 결측치 확인 

# 데이터 형태 맞춰주기 ---------------------------------
dataset$점수 <- as.numeric(dataset$점수)
dataset$공감 <- as.numeric(dataset$공감)
dataset$비공감 <- as.numeric(dataset$비공감)
dataset$날짜 <- ymd(dataset$날짜)
dataset$시간 <- factor(dataset$시간, levels=c(0:23), order=T)
dataset$요일 <- factor(dataset$요일, levels=c("월","화","수","목","금","토","일"), order=T)
dataset$평점그룹 <- ifelse(dataset$점수 >= 9, "A", ifelse(dataset$점수 <= 5, "C", "B"))
dataset$주 <- ifelse(dataset$요일 %in% c("토","일"), "주말", "주중")
str(dataset)

# 점수별 리뷰 분포 확인 
dis <-dataset %>%
  group_by(점수) %>% 
  summarise(리뷰수 = n()) %>% as.data.frame()

ggplot(dis, aes(x=점수, y=리뷰수)) +
    geom_bar(stat="identity", fill=rainbow(10)) +
    geom_text(aes(y=리뷰수+500, label=리뷰수), family='dohyeon', size=3.5) +
      scale_x_continuous(breaks = c(seq(0,10,1))) +
    theme_bw(base_size = 12, base_family = "jalnan") +
    labs(title = "점수별 리뷰 분포", y = "리뷰 수(단위:건)") +
    theme(plot.title = element_text(hjust = 0.5))

hist(dataset$점수)
table(dataset$점수)

# 일별별 평점 비교 -----------------------------------------------------
all_date_point <- dataset %>%
  group_by(날짜, 요일) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())

# 일별 평점 평균
ggplot(all_date_point,aes(x=날짜)) +
  geom_line(aes(y=date_mean_point), color="grey20", linetype=2) +
  geom_point(aes(y=date_mean_point), color=rainbow(43), size=3, alpha=.8) + 
  geom_text(aes(y=date_mean_point+0.02, label=round(date_mean_point,2)), family="dohyeon", size=3) +
  coord_cartesian(ylim = c(9, 9.8)) +
    theme_bw(base_family = "jalnan", base_size = 12) +
    labs(y="평균평점", title="일별 평점평균 변화", subtitle="2019-05-23 ~ 2019-07-04") +
    theme(legend.title = element_blank(),
          plot.title = element_text(hjust=0.5, size=20, family = "jalnan"),
          plot.subtitle = element_text(hjust=0.5, color="grey20"),
          axis.text.x = element_text(angle = 90, vjust=0.5)) +
    scale_x_date(breaks = all_date_point$날짜, labels=str_sub(all_date_point$날짜,7,10)) 
     

# 일별 댓글 수 
ggplot(all_date_point, aes(x=날짜, y=댓글수)) +
  geom_col(fill="antiquewhite", alpha=0.7, color="grey20") + 
  geom_text(aes(y=댓글수+30, label=댓글수), color='grey20', family="jalnan") + 
  geom_text(data = filter(all_date_point, 요일 %in% c("토", "일")), aes(y=100, label=요일), color="red", family="jalnan") + 
    theme_bw(base_family = "jalnan", base_size = 15) +
    labs(title="일별 댓글 수",y="댓글 수(단위:건)", subtitle="2019-05-23 ~ 2019-07-04") +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5, color="grey20"),
          axis.text.x = element_text(angle = 90, vjust=0.5),
          axis.text.y = element_blank()) +
    scale_x_date(breaks = all_date_point$날짜, labels=str_sub(all_date_point$날짜,7,10)) 

# 요일별 ----------------------------------
wday_point <- dataset %>% 
  group_by(요일) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())

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


# 시간대별 평점평균 변화  --------------------------------------------------------
all_time_point <- dataset %>% # 데이터 생성 
  group_by(시간) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())

ggplot(all_time_point,aes(x=시간)) +
  geom_point(aes(y=date_mean_point, color=rainbow(24), size=2)) +
  geom_segment(aes(x=시간,xend=시간,y=0,yend=date_mean_point), color="grey50") +
  coord_cartesian(ylim = c(9, 10)) +
    theme_bw(base_family = "jalnan", base_size = 15) +
    theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=20, family = "jalnan")) +
    labs(y="평점평균", title="시간대별 평점평균 변화") +
  geom_text(aes(y=date_mean_point+0.05, label=round(date_mean_point,2), family="dohyeon"))

# 시간대별 댓글 수
ggplot(all_time_point, aes(x=시간, y=댓글수)) +
  geom_col(fill="gold2", alpha=0.7, color="grey20") + 
  geom_text(aes(y=댓글수+30, label=댓글수), color='grey20', family="jalnan") + 
  theme_bw(base_family = "jalnan", base_size = 15) +
  labs(title="시간대별 댓글 수",y="댓글 수(단위:건)", subtitle="2019-05-23 ~ 2019-07-04") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color="grey20"),
        axis.text.x = element_text(vjust=0.5),
        axis.text.y = element_blank()) 

# 평점이 낮은 그룹 시간대별 평점평균 및 버즈량 ------------------------------------------- 
A_group <- filter(dataset, 평점그룹 == "A")
B_group <- filter(dataset, 평점그룹 == "B") 
C_group <- filter(dataset, 평점그룹 == "C") 

C_time_point <- C_group %>% # 데이터 생성 
  group_by(시간) %>%
  summarise(date_mean_point = mean(점수),
            공감수 = sum(공감),
            비공감수 = sum(비공감),
            댓글수 = n())

ggplot(C_time_point,aes(x=시간)) +
  geom_point(aes(y=date_mean_point, color=rainbow(24), size=2)) +
  geom_segment(aes(x=시간,xend=시간,y=0,yend=date_mean_point), color="grey50") +
  coord_cartesian(ylim = c(1.5, 3)) +
  theme_bw(base_family = "jalnan", base_size = 15) +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, size=20, family = "jalnan")) +
  labs(y="평점평균", title="시간대별 평점평균 변화") +
  geom_text(aes(y=date_mean_point+0.05, label=round(date_mean_point,2), family="dohyeon"))

ggplot(C_time_point, aes(x=시간, y=댓글수)) +
  geom_col(fill="red", alpha=0.3, color="grey20") + 
  geom_text(aes(y=댓글수+1, label=댓글수), color='grey20', family="jalnan") + 
  theme_bw(base_family = "jalnan", base_size = 15) +
  labs(title="평점이 낮은 그룹(5점이하)의 시간대별 댓글 수",y="댓글 수(단위:건)", subtitle="2019-05-23 ~ 2019-07-04") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color="grey20"),
        axis.text.x = element_text(vjust=0.5),
        axis.text.y = element_blank()) 

# 워드클라우드 --------------------------------------------
install.packages("rJava")
library(rJava) 
library(KoNLP)
library(dplyr)
library(stringr)
library(wordcloud2)
library(reshape2)

useSejongDic() # 세종사전 호출 

## 점수대(그룹)별 워드클라우드 --------------------------------------
clean <- function(text) {
  text <- as.character(text)
  text2 <- sapply(text, extractNoun, USE.NAMES = F) 
  text3 <- unlist(text2) %>% str_match("([가-힣]+)")
  text4 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 7},text3)
}

length(dataset$리뷰) # 총 리뷰수 18604 
A_review <- clean(A_group$리뷰) # 9~10점
B_review <- clean(B_group$리뷰) # 6~8점
C_review <- clean(C_group$리뷰) # 5점이하

write.table(A_review,"알라딘_리뷰_전처리_후_A.txt", row.names=F, col.names=F, quote=F) # 임시저장
write.table(B_review,"알라딘_리뷰_전처리_후_B.txt", row.names=F, col.names=F, quote=F) # 임시저장
write.table(C_review,"알라딘_리뷰_전처리_후_C.txt", row.names=F, col.names=F, quote=F) # 임시저장

# A 그룹의 워드클라우드 
A_text <- readLines("알라딘_리뷰_전처리_후_A.txt")
head(sort(table(A_text), decreasing = T),50)

A_text <- gsub("재밌어요","재미", A_text)
A_text <- gsub("재밌고","재미", A_text)
A_text <- gsub("재밌게","재미", A_text)
A_text <- gsub("쟈스민","자스민", A_text)
A_text <- gsub("지니가","지니", A_text)
A_text <- gsub("윌스미스가","윌스미스", A_text)
A_text <- gsub("월스미스","윌스미스", A_text)
A_text <- gsub("관람객너무","관람객", A_text)

gsub_txt <- readLines("gsubfile_A.txt")
for (i in 1:length(gsub_txt)) {
  A_text  <- gsub((gsub_txt[i]),"",A_text)
}

A_text <- A_text[A_text != ""] 
A_wc <-head(sort(table(A_text), decreasing = T),50)
wordcloud2(A_wc, size=0.7, 
           col="random-light", backgroundColor="black",
           fontFamily='나눔바른고딕')

# C 그룹의 워드클라우드
C_text <- readLines("알라딘_리뷰_전처리_후_C.txt")
head(sort(table(C_text), decreasing = T),50)

C_text <- gsub("애니메이션을","애니메이션",C_text)
C_text <- gsub("애니메이션이","애니메이션",C_text)
gsub_txt <- readLines("gsubfile_C.txt")
for (i in 1:length(gsub_txt)) {
  C_text  <- gsub((gsub_txt[i]),"",C_text)
}

C_text <- C_text[C_text != ""] 
C_wc <-head(sort(table(C_text), decreasing = T),50)
C_wc

wordcloud2(C_wc, size=0.7, 
           col="random-light", backgroundColor="black",
           fontFamily='나눔바른고딕')



