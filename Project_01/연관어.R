text2
library(tm)
cps <- Corpus(VectorSource(text))  

words <- function(data){
  text <- as.character(data)
  text2 <- sapply(text, extractNoun, USE.NAMES = F) 
  text3 <- unlist(text2) %>% str_match("([가-힣]+)")
  text4 <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 7},text4)
  gsub_txt <- readLines("gsubfile.txt")
  for (i in 1:length(gsub_txt)) {
    text5  <- gsub((gsub_txt[i]),"",text5)
  }
}

tdm <- TermDocumentMatrix(cps, control=list(tokenize=ko.words,   ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(4, 10),  
                                       weighting=weightBin))  

dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix)) <- "UTF-8"
rownames(tdm.matrix)[1:100]

word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출
co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

co.matrix

install.packages("qgraph")
library(qgraph)

qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       vsize=log(diag(co.matrix))*2) ##diag는 matrix에서 대각선만 뽑는 것임. 즉 그 단어가 얼마나 나왔는지를 알 수 있음. vsize는 그 크기를 결정하는데 여기 인자값으로 단어가 나온 숫자를 넘겨주는 것임. log를 취한것은 단어 크기의 차이가 너무 커서 log를 통해서 그 차이를 좀 줄여준것임. 



