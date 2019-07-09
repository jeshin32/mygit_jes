
# Modeling Process with Caret package -------------------------------------


# data load ---------------------------------------------------------------

library(DBI)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbListTables(con)

# make table to memory
dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)

dbListFields(con, "mtcars")
dbReadTable(con, "mtcars")

# Send Query 
df <- dbGetQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906/RAdvancedFunctions/Data/demo.sqlite")
dbListTables(con)

# Send Query 
df <- dbGetQuery(con, "SELECT * FROM albums limit 10")
df


# data merge --------------------------------------------------------------

library(dplyr)
library(data.table)
library(purrr)

#getwd()
#setwd("C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906")

my_db <- src_sqlite("Data/temp.sqlite", create = TRUE)

con <- dbConnect(RSQLite::SQLite(), "Data/temp.sqlite")
dbListTables(con)

# Delete table if exists
if ("iris" %in% dbListTables(con)) {
  dbRemoveTable(con, "iris")
}

dbWriteTable(con, "iris", iris)
dbListTables(con)

# merge
files <- c("file1.csv", "file2.csv","file3.csv")
folder <- "Data"
key <- "ID"

lst <- lapply(files, function(fn){fread(paste0("RCaret/Data/",fn))})
merge(lst[[1]], lst[[2]], key=key, all=T) # all.x = left outer join / all.y = right outer join
reduce(lst, merge, key=key, all=T)

# total <-  reduce( lapply(files, function(fn, folder) {
#   fread(paste0(folder,"/",fn))
# }, folder), merge, by= key, all.x = T)
# 
# print(total)



# data load2 --------------------------------------------------------------

raw <- read.csv("RCaret/Data/taitanic/train.csv")
library(stringr)
names(raw) <- str_to_lower(names(raw))

# factor화
raw$passengerid <- as.factor(raw$passengerid)
raw$survived <- as.factor(raw$survived)
raw$pclass <- as.factor(raw$pclass)

levels(raw$survived) = c("die","survived")
str(raw)


# initial modeling --------------------------------------------------------

nSample <- 500

set.seed(1)
ind <- sample(1:nrow(raw), nSample)
train <- raw[ind, ]
test <- raw[-ind, ]
dim(train)
dim(test)

library(randomForest)
library(caret)

set.seed(1)
initModel <- randomForest(survived ~ . - passengerid - name - ticket - cabin  ,
                          data = train, ntree = 200,
                          na.action = na.roughfix)

pred <- predict(initModel, test)
confusionMatrix(pred, test[, "survived"])

initModel$importance


# EDA ---------------------------------------------------------------------

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)

# example
transparentTheme(trans = .4)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

# preprocessing
library(ggplot2)

# 분석을 하지 않을 변수명 
notVar <- c("survived", "name", "passengerid", "ticket")
# 숫자형 변수명 
numVar <- names(raw)[sapply(raw, is.numeric)]
numVar
# 범주형 변수명 
cateVar <- setdiff(setdiff(names(raw), numVar), notVar)
cateVar
library(purrr)
cateVar <- reduce(list(names(raw), numVar, notVar), setdiff)
cateVar

source("Source/HelpFunctions.R")

for (var in numVar) {
  cat(var, "에 대한 요약입니다 ------------------------------- \n\n")
  aVar <- raw[, var, drop = F]
  # 기초 통계
  print(summary(aVar))
  # Histogram
  g1 <- ggplot(aVar, aes_string(var)) + geom_histogram(fill="skyblue", color="darkblue")
  g2 <- ggplot(aVar, aes_string(y=var)) + geom_boxplot()
  multiplot(g1, g2, cols=2)     # source에 포함된 가져온 함수
  cat("\n\n")
}

for (var in cateVar) {
  cat(var, "에 대한 요약입니다 ------------------------------- \n\n")
  aVar <- raw[, var, drop = F]
    # 기초 통계
  print(table(aVar))
  # Histogram
  g <- ggplot(aVar, aes_string(var)) + geom_bar(fill="skyblue", color="darkblue")
  plot(g)
  cat("\n\n")
  
}

yVar = "survived"
Y <- raw[, yVar, drop=F]

for (var in numVar) {
  X <- raw[, var, drop = F]
  # 연속형 변수를 범주화
  cutX <- cut(as.matrix(X), breaks = 7)
  freq <- table(data.frame(cutX, Y), useNA = "ifany")  
  freq_prop <- prop.table(freq, margin = 1) # 각 구간별 Y의 비율을 계산한다. 
  class(freq_prop)
  df_prop <- as.data.frame(freq_prop)
  # 범주별 Y의 비중을 시각화
  g <- ggplot(df_prop, aes(cutX, Freq, fill=survived)) + geom_bar(stat="identity") +
    xlab(var)
  print(g)
}


for (var in cateVar) {
  cat(yVar, "와", var, "의 관계에 대한 요약입니다 ------------------------------- \n\n")
  X <- raw[, var, drop = F]
  freq <- table(data.frame(X, Y), useNA = "ifany")
  # 각 구간별 Y의 비율을 계산한다. 
  freq_prop <- prop.table(freq, margin = 1)
  print(cbind(freq, freq_prop))
  class(freq_prop)
  df_prop <- as.data.frame(freq_prop)
  # df_prop
  # 범주별 Y의 비중을 시각화
  g <- ggplot(df_prop, aes_string(var, "Freq", fill="survived")) + geom_bar(stat="identity") +
    xlab(var)
  print(g)
}


# data pre-processing -----------------------------------------------------

library(caret)

# sampling을 모집단과 비슷하게 하여야 확률값에 대한 비슷한 수준을 맞출 수 있음

# simple random split
ind <- sample(1:nrow(data),  nrow(data) * 0.7)

train <- data[ind, ]
test <- data[-ind, ]

# splitting based on the outcome
ind <- createDataPartition(data$survived, p = .7, list = FALSE)

train <- data[ind, ]
test <- data[-ind, ]

# example - iris
data <- iris
ind <- createDataPartition(data$Species, p= 0.7, list=F)
train <- data[ind, ]
test <- data[-ind, ]


# splitting based on the predictors
startSet <- sample(1:nrow(temp), 5)
samplePool <- temp[-startSet,]
start <- temp[startSet,]

# 초기치와 모든 값의 거리를 구해서 뽑아낸다
# regression에서 특별 유용
ind <- maxDissim(start, samplePool, n = 20)



# remove near zero variance -----------------------------------------------

data(mdrr)    # in caret package
dim(mdrrDescr)
str(mdrrDescr)

# 0이라는 값이 전체 528개 중에 501개를 차지해서 너무 많은 비중을 지님
table(mdrrDescr$nR11)
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
head(nzv)


# Near Variance인 변수를 제거하는 방법 
dim(mdrrDescr)
nzv <- nearZeroVar(mdrrDescr)    # 제로 분산에 해당하는 변수의 Index를 반환 
nzv

filteredDescr <- mdrrDescr[ , -nzv]
dim(filteredDescr)

# https://thebook.io/006723/ch09/02/03/01/


# imputation --------------------------------------------------------------

# NA - delete or change ? zero ? 
# 행을 지우는 거는 매우 조심하게 해야 함
# 모형에 최대한 영향을 주지 않게끔 다른 값으로 채워 넣는 것

h2o.init()
h2o.getConnection() 
library(caret)

# medianImpute
# preprocess model <- logic에 대해 적용'만' 할 수 있어야 한다
preProcModel <- preProcess(train, method = c("medianImpute"))
preProcModel <- preProcess(train, method = c("knnImpute"))

trainP <- predict(preProcModel, train)
testP <- predict(preProcModel, test)

# example

tr <- data.frame(
  A=c(1,2,NA,4,5),
  B=c(100,100,100,200,200)
)
te <- data.frame(
  A=c(NA,NA),
  B=c(100,200)
)

preProcModel <- preProcess(tr, method = c("medianImpute"))

trainP <- predict(preProcModel, tr)
testP <- predict(preProcModel, te)


# PCA ---------------------------------------------------------------------
# x data끼리의 관계가 클 때 사용함
# mnist case: 글씨 주변 쓸데없는 테두리 날림
# image PCA : 속도 개선

# pca : 연속형 변수에 대해서만 자동으로 PCA transformation을 적용해 줌 
preProcModel<- preProcess(iris, method = c("pca"), pcaComp = 3) 

# PCA 변환 수행 
irisPCA <- predict(preProcModel, iris)
head(irisPCA)

# 변환전 데이터와 PCA 변환 후 데이터 그리기 
g1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(size=5, alpha =0.5) + ggtitle("Original Axis") +
  theme(legend.position="bottom")

g2 <- ggplot(irisPCA, aes(PC1, PC2, color=Species)) +
  geom_point(size=5, alpha =0.5) + ggtitle("Two PCA Axis") +
  theme(legend.position="bottom")

# setwd("C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906/RCaret")
source("Source/HelpFunctions.R")
multiplot(g1, g2, cols=2)


# put it all together -----------------------------------------------------

data <- read.csv("Data/taitanic/train.csv")
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$temp_zv <- 7

summary(data)

ind <- sample(1:nrow(data),  nrow(data) * 0.7)
train <- data[ind, ]
test <- data[-ind, ]

# Remove Zero Variance Variable & using knn Imutaion & pca transformation
preProcModel <- preProcess(train[, -2], method = c("zv", "knnImpute", "pca"), princomp = 4)
preProcModel

trainP <- predict(preProcModel, train)
head(trainP,20)
summary(trainP)

testP <- predict(preProcModel, test)


# model development -------------------------------------------------------

library(caret)

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
gbmFit2


