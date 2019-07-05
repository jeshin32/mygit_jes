#install.packages('h2o')
library(h2o)
h2o.init()


# 데이터 읽어오기 ----------------------------------------------

job.titles.path = "https://raw.githubusercontent.com/h2oai/sparkling-water/rel-1.6/examples/smalldata/craigslistJobTitles.csv"

job.titles <- h2o.importFile(job.titles.path, destination_frame = "jobtitles",
                             col.names = c("category", "jobtitle"), col.types = c("Enum", "String"), header = TRUE)
job.titles


# 1.1.1 형태소분해  ----------------------------------------------

# 의미가 없어 제외하고자하는 단어의 집합을 정의 (영어의 경우, 전치사 관사 대명사  등을 제거)
STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
               "there","all","we","one","the","a","an","of","or","in","for","by","on",
               "but","is","in","a","not","with","as","was","if","they","are","this","and","it","have",
               "from","at","my","be","by","not","that","to","from","com","org","like","likes","so")

# 이미 만들어진 형태소 분해 함수 (가져다 쓴다)
tokenize <- function(sentences, stop.words = STOP_WORDS) {
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  # remove short words (less than 2 characters)
  tokenized.lengths <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  # remove words that contain numbers
  tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  
  # remove stop words
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

# 형태소 분해 수행 : Break job titles into sequence of words
words <- tokenize(job.titles$jobtitle)
dim(words)

as.vector(words)[1:100]

# 1.1.2 Data Explore    ----------------------------------------------
library(data.table)
library(dplyr)

w.dt <- as.data.table(words)

# NA를 제외한 words의 빈도를 구한다. 
word.freq <- w.dt[!is.na(C1), .N, by = C1 ]
colnames(word.freq) <- c("word", "N")

# Sort한다. 
setorder(word.freq, -N)
data.table(word.freq)

# Wordcloud를 그린다. 
library(wordcloud2)

wordcloud2(data = word.freq, minSize=10)


# 1.2 Text Mining Modeling    ----------------------------------------------

# 각각의 word를 100차원의 벡터로 숫자화 시키는 words2vec 모형 수행
w2v.model <- h2o.word2vec(words, vec_size = 100, epochs = 10)

#  words를 벡터로 변환 : "assistent"라는 word를 100차원 벡터로 변환 
vec1 <- h2o.transform(w2v.model, as.h2o(word.freq[1,1]))
vec1

# 30개의 대표 words를 벡터로 변환
vec <- h2o.transform(w2v.model, as.h2o(word.freq[1:30,1]))
dim(vec)

View(as.data.frame(vec))

# PCA를 쉽게 수행할 수 있도록 도와 주는 패키지
# install.packages('FactoMineR')
library("FactoMineR") 

# 변환된 vec 데이터를 data로 저장
data <- as.data.frame(vec)
rownames(data) <- word.freq$word[1:30]   # 이름 붙이기

# 2차원으로 그리기 위해, PCA를 수행
pca <- PCA(data, graph = FALSE)
plot(pca, choix = "ind", cex = 1.2)

# teacher와 유사한 단어를 찾아본다
h2o.findSynonyms(w2v.model, "finance", count = 10)


# 1.2.2 Classificatoin Model    ----------------------------------------------

# 각 Job Title을 숫자화 함 
job.title.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")
dim(job.titles)

dim(job.title.vecs)


# 데이터 준비 
valid.job.titles <- ! is.na(job.title.vecs$C1)   # NA로 되어 있는 job.title.vec는 제거 
data <- h2o.cbind(job.titles[valid.job.titles, "category"], job.title.vecs[valid.job.titles, ])
data.split <- h2o.splitFrame(data, ratios = 0.8) # list 2개로 반환한다

# GBM model 개발 
gbm.model <- h2o.gbm(x = names(job.title.vecs), y = "category",
                     training_frame = data.split[[1]], validation_frame = data.split[[2]])


# 예측 모듈 함수화 : 텍스트와 w2v, gbm 모듈을 받아 카테고리 예측 결과를 제공한다. 
predict <- function(job.title, w2v, gbm) {
  words <- tokenize(as.character(as.h2o(job.title)))
  job.title.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  h2o.predict(gbm, job.title.vec)
}

library(tidyr)
library(ggplot2)


# Example 1
out <- predict("school teacher having holidays every month", w2v.model, gbm.model)
out

long <- gather(as.data.frame(out[,-1]))
long

ggplot(long, aes(x=key, y=value)) + geom_bar(stat="identity")

# Example 2
out <- predict("developer with 3+ Java experience, jumping", w2v.model, gbm.model)


long <- gather(as.data.frame(out[,-1]))
long

ggplot(long, aes(x=key, y=value)) + geom_bar(stat="identity")

# Example 3
out <- predict("insurance, car", w2v.model, gbm.model)


long <- gather(as.data.frame(out[,-1]))
long

ggplot(long, aes(x=key, y=value)) + geom_bar(stat="identity")
