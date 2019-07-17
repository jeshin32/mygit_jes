

# review ------------------------------------------------------------------

# advanced func

# apply == for loop
apply(iris[,1:4],2,mean) # 1=row, 2=column
lapply(iris,class) # lapply(list or vector, function), output=list , fast
sapply(iris, is.numeric) # output=vector or table, fast
tapply(iris$Sepal.Length, iris$Species, mean)
sweep(iris[,1:4],2,colMeans(iris[,1:4]),"-")

# files
file.exists(filepath)

# string
substr("abcdef",2,4) # 위치 index기준
library(stringr)
word("I have a coffee", 2:4) # 공백으로 구분된 단어기준으로 필요한 위치를 뽑아줌
word("I have a coffee", 1:4)
grep("A",c("b","A","Apple"), invert=T) # value=F : location, value=T : names / invert=T : 반대 case
sub(" ", ",", "Hello World")


# random number
runif(4)
runif(4,min=0,max=100)
rnorm(4,mean=50,sd=10)
sample(1:100, 3, replace=T)

# set func
"D" %in% c("A","D")
setdiff() # 차집합
setequal() # 두 개의 집합이 동일한지 확인
cut(runif(100), breaks=10)

# cut example
set.seed(1)
test <- data.frame(score = sample(1:100, 1000, replace=T))
head(test)
test$level <- cut(test$score, breaks=seq(0,100,by=20), labels=c("가", "양", "미", "우", "수"),
                  include.lowest=T) # ( ] 초과/포함
head(test)

library(dplyr)
test %>% group_by(level) %>% summarise(n())


# purrr
library(purrr)
rerun(5,rnorm(5))
reduce(1:100, `+`)
reduce(c("a","b","c"), paste0) # file merge
reduce_right(c("a","b","c"), paste0)

# etc
fomula() # text로 만든 수식을 통계함수에 넣을 때
system.time() # 수행속도
Sys.time() # 현재 시간
cumsum(1:3) # 누적 vectors
plyr::mapvlues() == dplyr::recode()

# split 그룹별롣 데이터셋을 쪼갬
require(stats) ; require(graphics)
n <- 10 ; nn <- 100
g <- factor(round(n*runif(n*nn)))
x <- rnorm(n*nn) + sqrt(as.numeric(g))
x
xg <- split(x,g)
xg

split(iris,iris$Species) # 실제로 데이터를 쪼개서 리스트에 담고 있음

g <- sample(1:3, 150, replace=T)
g
split(iris,g) # random 쪼개기

sample_n() # n개 행을 뽑아줌
sample_frac() # % 행을 뽑아줌
quantile() # 순위에 따른 등수 매기기 할 때 유용

# quantile example
test$level2 <- cut(test$score, breaks=quantile(test$score, seq(0,1, by=0.2)), labels=c("가", "양", "미", "우", "수"),
                  include.lowest=T) # ( ] 초과/포함
head(test)

# expandRows
library(data.table)
mydf <- data.frame(x=c("a","b","c"),
                   y=c("d","e","f"),
                   count=c(2,3,5))
mydf



# review advanced dplyr ---------------------------------------------------

y %>% func(x, ., z)

# filter
filter_all()
filter_if() # 컬럼 선택은 아닙니다, 조건만 걸러줄 뿐
filter_at() # 조건을 넣어줄 위치를 설정

iris %>% 
  select_if(is.numeric) %>% 
  filter_all(any_vars(. > 4.5))

# transmutate() : 변환된 결과만을 반환
iris %>% transmute(SL=Sepal.Length*10)

iris %>% group_by(Species) %>% add_count() # 그룹별 카운트한 숫자를 n으로 붙여줌

# DB
library(DBI)
library(RSQLite)
library(dbplyr)

# %$% 변수이름만 가지고 작업하기
library(magrittr)
iris %$% cor(Petal.Length, Petal.Width)

x <- 1:10
x %<>% log %>% round(2) 
x

rnorm(5) %T>% print %>% mean 
iris %>%  select_if(is.numeric) %T>% head %>% colMeans # 이거 안 넘어가....



# review caret package ----------------------------------------------------

install.packages('writexl')
library(writexl)   # library(readxl)

write_xlsx(iris, path="data/aaa.xlsx")
write_xlsx(list(iris,mtcars), path="data/aaa_list.xlsx")

reduce(lapply(files,
              function(fn, folder) {fread(paste0(folder,"/",fn))},
              folder),merge, by=key, all.x=T)

# practice
getwd()

links <- "C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906/RCaret/Data"
files <-list.file(links,"^file") # c("file1.csv", "file2.csv", "file3.csv")

lst <- lapply(files, function(fn) {fread(paste0(links,"/",fn))})
merge_123 <- reduce(lst, merge, by="ID", all.x=T)
merge_123                    

?reduce


# basic function
dim(iris)
names(iris)
head(iris)
str(iris)
summary(iris)
glimpse(iris)


# randomforest
# na.action = na.roughfix

# n-fold cv : 소요시간이 n배 든다는 문제가 항상 발생 (2~3 folds recommended)
library(caret)
idx <- createDataPartition(data$survived, p=.7, list=FALSE) # 이건 y값 기준으로
train <- data[idx, ]
test <- data[-idx, ]

# but we should cover all x ranges (그래프가 어찌 변할지 모름)
# 초기값 뽑고, 전체 거리를 계산해서 평균거리가 먼 아이들을 차례로 random sampling
# validation & test 개선
# 하지만 오래걸류....  다 계산해야 하니까

startset <- sample(1:nrow(temp),5)
samplePool <- temp[-startset]
start <- temp[startset, ]

idx_x <- maxDissim(start, samplePool, n=20)

# zero Variance (Constant Value) : 값이 1개이기 때문에 정보가 없다, 회귀&분류 계산에 기어하지 못한다
# near zero variance : 실제 의미 있을 확률 높으므로 실무 정보를 접목해야 한다



# imputation --------------------------------------------------------------

# missing을 하나의 트리구조에 심어둘 수 있기도 하긴 하다
# training data 만을 가지고 처리하는 것
# test set missing 이라도 training data에 있는 정보를 활용

# PCA : 가장 많은 정보를 가지고 있는 축 : 가장 분산이 큰 방향 (가장 넓게 펼쳐져 있는 방향)
# 빠른 예측, noise reduction, 상관관계가 큰 data에서 잘 작용한다
# 시각화에 많이 쓰인다

preProcmodel <- preProcess(iris,method=c("pca"), pcaComp = 3)
irisPCA <- predict(preProcmodel,iris)
head(irisPCA)



# Put it all together -----------------------------------------------------

setwd("C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906/RCaret")
data <- read.csv("Data/taitanic/train.csv")
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$temp_zv <- 7

summary(data)

ind <- sample(1:nrow(data),  nrow(data) * 0.7)
train <- data[ind, ]
test <- data[-ind, ]

# Remove Zero Variance Variable & using knn Imutaion & pca transformation
# not modeling data preprocessing

install.packages('RANN')
library(RANN)

preProcModel <- preProcess(train[, -2], method = c("zv", "knnImpute", "pca"), princomp = 4)
preProcModel

# mean centering : 평균을 0으로 만든다 (x - mew)
# auto scaling : 각 변수의 분산을 동일하게 만들어 준다 ((x-mew)/sigma) shape은 변화가 없고, 변화의 폭을 바꿔줌
# knn 거리측정 >> 변수값이 큰 거에 영향을 많을 수 밖에 없으므로 기본적으로 auto scaling 해준다

trainP <- predict(preProcModel, train)
head(trainP,20)
summary(trainP)

testP <- predict(preProcModel, test)
testP

# 새로운 변수 PC : 0.3x1 + 0.6x2 (변수들의 linear combination) - feature loading value
# weight값을 확인하여 어떤 속성을 가지고 있는지 확인한다


# imputation2 -------------------------------------------------------------

# 2.5? 5.1? 무엇이 정답일까?
train <- data.frame(
  A = c(1,2,3,4,5),
  B = c(1.1,2.1,3.1,4.1,NA),
  c = 7 )
train
test <- data.frame(
  A = c(4,5),
  B = c(NA, 4.9),
  c = 7 )
test

# median
preM <- preProcess(train, c("medianImpute"))
trainP <- predict(preM, train)
trainP
testP <- predict(preM, test)
testP

# knn
preK <- preProcess(train, c("zv", "knnImpute"), k=1)
trainP2 <- predict(preK, train)
trainP2 # centering, autoscaling은 자동으로 실행함
testP2 <- predict(preK, test)
testP2


# tuning grids ------------------------------------------------------------

# library(caret)

ind <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[ind, ]
valid <- iris[-ind, ]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)  

set.seed(1)
gbmFit1 <- train(Species ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)  # This last option is actually one
# for gbm() that passes through
gbmFit1

gbmGrid <-  expand.grid(
  interaction.depth = c(1, 5, 9), 
  n.trees = (2:10) * 50, 
  shrinkage = 0.1,
  n.minobsinnode = c(10, 20)
)
nrow(gbmGrid)

set.seed(1)
gbmFit2 <- train(Species ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2 # suggestion check

trellis.par.set(caretTheme())
plot(gbmFit2) 

plot(gbmFit2, plotType = "level",
     scales = list(x = list(rot = 90)))

ggplot(gbmFit2)

# accuracy : bias data에서 문제 >> precision(예측의 정밀도) recall(이상징후의 감지)
# f1 score : precision과 recall의 harmonic mean (정밀도와 재현율의)



# Alternate Tuning Grids --------------------------------------------------

# R = SINGLE CORE
install.packages('doParallel')
library(doParallel)
cl <- makePSOCKcluster(2)
registerDoParallel(cl)
  
## When you are done:
stopCluster(cl)

# foreach
#install.packages('foreach')
library(foreach)

# Manual Search : for loop
modellist <- list()

for (ntree in c(200, 500, 1000)) {
  for(nodesize in c(5,10,20)) {
    set.seed(2)
    fit <- train(Species~., 
                 data=iris, 
                 method="rf", 
                 metric=metric, 
                 tuneGrid=grid, 
                 trControl=control, 
                 ntree=ntree,
                 nodesize = nodesize)
    
    key <- paste(ntree, nodesize, sep="_")
    modellist[[key]] <- fit    # 리스트에 담는 부분
  }
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

# classification >> 확률값을 도출하여 분류하는 것
# cut.off = 0.5? >> accuracy, precision, recall, F1에 따라서 


# lift & gain -------------------------------------------------------------
# gain : lift의 누적

library(randomForest)

data <- read.csv("Data/taitanic/train.csv")
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$temp_zv <- 7

data.roughfix <- na.roughfix(data)
ind <- sample(1:nrow(data),  nrow(data) * 0.7)
train <- data.roughfix[ind, ]
test <- data.roughfix[-ind, ]

set.seed(1)
rf <- randomForest(Survived ~ . - PassengerId - Name - Ticket - Cabin, 
                   data = train, ntree=200,
                   trControl = fitControl,
                   na.action = na.roughfix)
rf

pred <- predict(rf, test, type="prob")

# calculate
# 내 data의 random에 비해서 뭔가 성능 개선을 일궈냈다는 사실이 포인트가 된다
pred_df <- data.frame(survived_p = pred[,2])
pred_df$answer <- test$Survived
head(pred_df)
pred_df$level <- cut(pred_df$survived_p, breaks=quantile(pred_df$survived_p, seq(0,1, by=0.1)),
                     labels=10:1, include.lowest=T)
pred_df$level2 <- cut(pred_df$survived_p, breaks=seq(0,1, by=0.1),
                     labels=10:1, include.lowest=T)
head(pred_df)
summary(test)

pred_df %>% group_by(level2) %>%
  summarise(n=n(),
            res=sum(ifelse(answer=="1",1,0))) %>%
  arrange(desc(level2)) %>% 
  mutate(surRatio = res/n,
         random = n*(97/268),
         lift = res/random,
         cumRes = cumsum(res),
         cumRandom = cumsum(random),
         cumLift = cumRes/cumRandom)

# missing = roc curve


# Make API using plumber --------------------------------------------------

install.packages('plumber')
library(plumber)


# Call 'plumber.R'
getwd()
r <- plumb("Source/plumber.r")

r$run(port=8000)

