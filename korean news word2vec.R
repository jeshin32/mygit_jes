# Use KoNLP ---------------------------------------------------------------
## java 1.8 path
## Sys.setenv("JAVA_HOME"=java_jdk_설치_경로)
## Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "java_jdk_설치_경로", sep = ":"))
library(rJava) # 오류 java path 설정
library(KoNLP) # 한국어 텍스트 처리
library(dplyr)
library(stringr) # 문자열 처리

## 사전 설정
# useSejongDic() # 단어량이 상대적으로 적음.
useNIADic()

## Test
example_sen <- '현대해상 조교는 재미가 있어요'
extractNoun(example_sen) # 명사만
MorphAnalyzer(example_sen) # 문장 전체 형태소 분석
SimplePos09(example_sen) 
# SimplePos22(example_sen)

# Load data ---------------------------------------------------------------

getwd()
news_raw<-read.csv("C:/Users/Uxxxxxxx/Downloads/example_hangul.csv",stringsAsFactors = F)
head(news_raw)
str(news_raw)

news_raw$category<-as.factor(news_raw$category) # cha -> factor
table(news_raw$category)
str(news_raw)

## 데이터 정제 
# stringr패키지의 str_replace_all(text, regexp, replace) 이용
txt <- str_replace_all(news_raw$content, "\\W", " ") # 알파벳,숫자를 제외한 모든 문자 띄어쓰기 단위 
head(txt)
txt <- str_replace_all(txt, "[[:digit:]]", "") # 숫자 제거 
head(txt)
txt <- str_replace_all(txt, "[[A-z]]", "") # 영어 제거 
head(txt)
txt <- str_replace_all(txt, "[[:punct:]]", "") # 문장부호와 기호 제거 
head(txt)
txt <- str_replace_all(txt, "  ", "") # doublespace 제거 
head(txt)

## nchar & Extract Noun
# 5min 소요
ext_noun <- sapply(txt, extractNoun) # 명사 추출
names(ext_noun) <- NULL # 리스트 이름이 너무 길어서 제거
head(ext_noun)

# 2~4글자 단어만 살려놓는 사용자 함수 생성 
len_char <- function(x){
  Filter(function(y){
    nchar(y) <= 4 && nchar(y) > 1
  },x)
}

final_noun <- sapply(ext_noun, len_char)
final_noun[[1]]


## Make input H2O, 영어 예제외 똑같은 형태로 만들기 위함.
pasted_noun <- lapply(final_noun, function(x){paste(x,collapse = " ")})
pasted_noun[[1]]

data<- news_raw %>% select(category) %>% mutate(noun = unlist(pasted_noun))
head(data)

# H2O ---------------------------------------------------------------------

library(h2o)
h2o.init(nthreads = -1)

# 올려져 있는 데이터를 h2o 형태로 변환
news_data <- as.h2o(data, destination_frame = "news_data")
class(news_data)
head(news_data) # 깨진채로 작업


# Tokenize ----------------------------------------------------------------

# 이미 명사가 다 추출이 되어있는 상태라 문장만 구분해 주면 됩니다.
words <- h2o.tokenize(news_data$noun," ") # 단어만 구분 띄어쓰기로로
head(words,40)
dim(words)

### 요부분은 돌리지마세요~
Sys.setlocale(category = "LC_ALL",locale = "us") # 깨진채로 다른 인코딩으로 변환(이후에 작업하는데 있어서 에러를 피하기 위해)
as.vector(words)[1:100]

# Data Explore ------------------------------------------------------------
# 
 library(data.table)
 w.dt <- as.data.table(words)
# 
# # NA를 제외한 words의 빈도를 구한다.
 word.freq <- w.dt[!is.na(C1), .N, by = C1]
 colnames(word.freq) <- c("word", "N")
# 
# # sort
 setorder(word.freq, -N)
 data.table(word.freq)

# Draw Wordcloud
# install.packages("wordcloud2")
# library(wordcloud2)
# wordcloud2(data = word.freq, minSize = 2)


# Training Word2Vec -------------------------------------------------------

# 1min 소요
w2v.model <- h2o.word2vec(words, vec_size = 100, epochs = 10) # window_size, min_word_freq
?h2o.word2vec

# words를 벡터로 변환 : 특정 단어 word를 100차원 벡터로 변환 
vec1 <- h2o.transform(w2v.model, as.h2o(word.freq[1,1]))
vec1

# 30개의 대표 words를 벡터로 변환
vec <- h2o.transform(w2v.model, as.h2o(word.freq[1:30,1]))
vec
dim(vec)

View(as.data.frame(vec))

# PCA를 쉽게 수행할 수 있도록 도와 주는 패키지
# install.packages("FactoMineR")
library(FactoMineR) 

# 변환된 vec 데이터를 data로 저장
data <- as.data.frame(vec)
rownames(data) <- word.freq$word[1:30]   # 이름 붙이기
head(data)

# 2차원으로 그리기 위해, PCA를 수행
pca <- PCA(data, graph = FALSE)
plot(pca, choix = "ind", cex = 1.2)

# 유사한 단어를 찾을 수가 없습니다
# h2o.findSynonyms(w2v.model, "벤츠", count = 1)


# Classification Model ----------------------------------------------------

# 각 news_content을 숫자화 함 
news.contents.vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")
dim(news.contents.vecs)

# 데이터 준비 
valid.news.contents <- ! is.na(news.contents.vecs$C1)   # NA로 되어 있는 news.contents.vecs는 제거 
final_data <- h2o.cbind(news_data[valid.news.contents, "category"], news.contents.vecs[valid.news.contents, ])
head(final_data)
dim(final_data)
final_data.split <- h2o.splitFrame(final_data, ratios = 0.8)

# GBM model 개발 
gbm.model <- h2o.gbm(x = names(news.contents.vecs), y = "category",
                     training_frame = final_data.split[[1]], validation_frame = final_data.split[[2]])
gbm.model


