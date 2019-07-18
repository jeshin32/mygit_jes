
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 0. 환경 설정 ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

rm(list = ls())
options(digits = 4)


# ** Package  설치  ---------------------------------------------------- 


# ** Library 설정  ----------------------------------------------------

#install.packages('ggthemes')
#install.packages('pROC')
#install.packages('mice')

library(randomForest)
library(caret)
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(data.table)
library(tidyr)
library(AppliedPredictiveModeling)
library(pROC)

# ** Source 설정   ----------------------------------------------------

HOME_PATH <- paste0(getwd(), "/")   # 현재 프로젝트 폴더를 홈으로 지정
HOME_PATH
SOURCE_PATH <- paste0(getwd(), "/SOURCE/") # 홈 밑에 SOURCE 폴더 지정
SOURCE_PATH
source(paste0(SOURCE_PATH, "Source_Func.R"), encoding = "UTF-8") 

# 하위 폴더를 모두 설정하고, 그 패스 경로를 변수명에 저장
folder_list <-  c("DATA", "RESULT", "MODEL")
makeDirStructure(HOME_PATH, folder_list)
RESULT_PATH



# ** 실행 파라미터 설정  ------------------------------------------------
#    : 글로벌 상수는 대문자로 표현하는 습관 

FILE_NAME <- "Data-Titanic.csv"   # 데이터 파일 이름
N_EDA <- 500                      # EDA를 수행할 Sample Size (if NULL, use all raw)
SEED <- 5                         # 재현성을 위한 Random number seed

P_TRAIN <- 0.7                    # Train Data Ratio, big data인경우에는 작게 시작하자  

Y_NAME <- "survived"              # Y Variable Name
REMOVE_NAMES <- c("passengerid", "name", "ticket", "cabin")     # variable names to remove in modeling


# Modeling Parameter
PRE_PROCESS <- c("zv", "center", "scale", "medianImpute")

FIT_CONTROL <- trainControl(method = "cv", number = 10)   ## 10-fold CV, 실전에서는 작게


# ' -----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 1. 데이터 수집 & Handling  ---------------- ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

printResult("전체 코드를 시작합니다 ")


# ** 파일에서 읽어오기  ------------------------------------------------

raw <- read.csv(paste0(DATA_PATH, FILE_NAME))

# ** 변수명을 모두 소문자로   ------------------------------------------------ 

names(raw) <- str_to_lower(names(raw))

N_DATA <- nrow(raw)
N_VAR <- ncol(raw)

printResult("데이터의 dimension ", dim(raw))

# ** Basic Variable Type 변환   ------------------------------------------------

printResult("raw 데이터의 변수 Type ", str(raw))

n2f_list <- c("passengerid", "survived", "pclass")     # 데이터에 따라 설정한다. 
for (var in n2f_list) {
  raw[, var] <- as.factor(raw[, var])
}

printResult("변환된 raw 데이터의 변수 Type ", str(raw))


# ** 데이터로 부터 Y 변수 만들기   ------------------------------------------------ 



# ** X 변수 기본 가공하기  ------------------------------------------------ 



# ** EDA를 위한 데이터 Sampling   ------------------------------------------------

if (is.null(N_EDA)) {
  eData <- raw
} else {
  set.seed(SEED) 
  ind <- sample(1:N_DATA, N_EDA)
  eData <- raw[ind, ]
}

dim(eData)


# ' -----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 2. 데이터 탐색 (EDA)  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

# ** Setting for EDA   ------------------------------------------------ 

ALL_NAMES <- names(raw)
X_NAMES <- setdiff(names(raw), Y_NAME)
X_NAMES <-  setdiff(X_NAMES, REMOVE_NAMES)

NUM_VARS <- names(raw)[sapply(raw, is.numeric)]  # 숫자형 변수
FACTOR_VARS <- setdiff(setdiff(names(raw), NUM_VARS), REMOVE_NAMES) # 범주형 변수

printResult("전체 변수 이름입니다", ALL_NAMES)

# ** Some Basic EDA Functions   ------------------------------------------------

printResult("기본 분석 입니다")

dim(eData)
names(eData)
head(eData)
str(eData)
summary(eData)

# ** Initial Modeling   ------------------------------------------------

set.seed(SEED)
ind <- createDataPartition(raw[, Y_NAME], p = P_TRAIN, list = FALSE)

train <- raw[ind, ]
test <- raw[-ind, ]

dim(train)
dim(test)

set.seed(SEED)

model_eq <- formula(paste(Y_NAME, "~", paste(X_NAMES, collapse = " + " ) ))
model_eq

initModel <- randomForest(model_eq  ,
                          data = train, ntree = 200,
                          na.action = na.roughfix)

pred <- predict(initModel, test)
conMat <- confusionMatrix(pred, test[, Y_NAME])

printResult("초기 모형의 Acuracy는 ", conMat$overall[1])
printResult("초기 모형의 Test Confusion Matrix ", conMat$table)
printResult("변수 중요도는 ", initModel$importance)

# ** EDA using Caret   ------------------------------------------------

# transparentTheme(trans = .4)
# library(caret)
# featurePlot(x = iris[, 1:4], 
#             y = iris$Species, 
#             plot = "pairs",
#             ## Add a key at the top
#             auto.key = list(columns = 3))

# ** Univariate EDA   ------------------------------------------------ 

printResult("Numeric 변수의 단변량 EDA ")

for (var in NUM_VARS) {
  
  cat(var, "에 대한 요약입니다 ------------------------------- \n\n")
  aVar <- eData[, var, drop = F]
  
  # 기초 통계
  print(summary(aVar))
  
  # Histogram
  g1 <- ggplot(aVar, aes_string(var)) + geom_histogram(fill="skyblue", color="darkblue")
  g2 <- ggplot(aVar, aes_string(y=var)) + geom_boxplot()
  
  multiplot(g1, g2, cols=2)     # source에 포함된 가져온 함수
  
  cat("\n\n")
  
}

printResult("Factor 변수의 단변량 EDA ")

for (var in FACTOR_VARS) {
  
  cat(var, "에 대한 요약입니다 ------------------------------- \n\n")
  aVar <- eData[, var, drop = F]
  numLevels <- length(levels(aVar[[1]]))
  
  if (numLevels <= 30) {
    # 기초 통계
    print(table(aVar[[1]]))
    
    # Histogram
    g <- ggplot(aVar, aes_string(var)) + geom_bar(fill="skyblue", color="darkblue")
    plot(g)
    
    cat("\n\n")
  } else {
    
    cat("이 변수의 Levels이 30개가 넘습니다.\n\n") 
    cat("level의 개수는", numLevels, "입니다.", "\n\n")
    
    print(table(aVar[[1]])[1:30])
    
    cat("\n\n")
  }
  
}

# ** EDA: Related to Y ------------------------------------------------ 

Y <- eData[, Y_NAME, drop=F]

printResult("Numeric X 변수의 Y와의 관계 분석 ")

for (var in NUM_VARS) {
  
  cat(var, "에 대한 요약입니다 ------------------------------- \n\n")
  
  X <- eData[, var, drop = F]
  
  # 연속형 변수를 범주화
  cutX <- cut(as.matrix(X), breaks = 7)
  
  freq <- table(data.frame(cutX, Y), useNA = "ifany")  
  freq_prop <- prop.table(freq, margin = 1) # 각 구간별 Y의 비율을 계산한다. 
  print(cbind(freq, freq_prop))
  
  df_prop <- as.data.frame(freq_prop)
  
  # 범주별 Y의 비중을 시각화
  g <- ggplot(df_prop, aes(cutX, Freq, fill=survived)) + 
    geom_bar(stat="identity") +
    xlab(var)
  
  print(g)
  
  cat("\n")
}

printResult("Factor X 변수의 Y와의 관계 분석 ")

for (var in FACTOR_VARS) {
  
  cat(Y_NAME, "와", var, "의 관계에 대한 요약입니다 ------------------------------- \n\n")
  X <- eData[, var, drop = F]
  numLevels <- length(levels(X[[1]]))
  
  if(numLevels <= 30) {
    
    freq <- table(data.frame(X, Y), useNA = "ifany")
    
    # 각 구간별 Y의 비율을 계산한다. 
    freq_prop <- prop.table(freq, margin = 1)
    print(cbind(freq, freq_prop))
    
    df_prop <- as.data.frame(freq_prop)
    
    # 범주별 Y의 비중을 시각화
    g <- ggplot(df_prop, aes_string(var, "Freq", fill="survived")) + geom_bar(stat="identity") +
      xlab(var)
    print(g)
    
  }
  
  cat("\n")
  
}


# ' -----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 3. 데이터 전처리  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

# ** X 데이터 변환  ----------------------------------------------------


# ** Imputation  ----------------------------------------------------



# ** 데이터 쪼개기   ----------------------------------------------------

set.seed(SEED)
ind <- createDataPartition(raw[, Y_NAME], p = P_TRAIN, list = FALSE)

# 모형에 들어갈 X로만 X 데이터 생성 
trainX <- raw[ind, X_NAMES]    
testX <- raw[-ind, X_NAMES]

# Y 데이터만 따로 생성 
trainY <- raw[ind, Y_NAME]
testY <- raw[-ind, Y_NAME]


# ** Data Preprocessing  ----------------------------------------------------

preProcModel <- preProcess(trainX, PRE_PROCESS)

printResult("Data Preprocessing이 수행되었습니다", preProcModel)

trainX_P <- predict(preProcModel, trainX)
testX_P <- predict(preProcModel, testX)


# ' -----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 4. 모델링 수행  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

# ** 모형 개발   ------------------------------------------

gbmGrid <-  expand.grid(
  interaction.depth = c(1, 5, 9), 
  n.trees = c(100,200),
  shrinkage = 0.1,
  n.minobsinnode = c(10, 20)
)
nrow(gbmGrid)

set.seed(SEED)
gbmFit <- train( trainX_P, trainY, 
                 method = "gbm", 
                 trControl = FIT_CONTROL, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)
gbmFit

trellis.par.set(caretTheme())
plot(gbmFit) 


rfGrid <-  expand.grid(
  mtry = c(2,4,7)
)
nrow(rfGrid)

set.seed(SEED)
rfFit200 <- train( trainX_P, trainY, 
                 method = "rf", 
                 trControl = FIT_CONTROL,
                 verbose = FALSE,
                tuneGrid = rfGrid,
                ntree=200)
rfFit200

trellis.par.set(caretTheme())
plot(rfFit200) 


set.seed(SEED)
rfFit400 <- train( trainX_P, trainY, 
                   method = "rf", 
                   trControl = FIT_CONTROL,
                   verbose = FALSE,
                   tuneGrid = rfGrid,
                   ntree=400)
rfFit400

trellis.par.set(caretTheme())
plot(rfFit400) 

# ** Best Model 선택  ------------------------------------------

# Cross-Vaidation 결과 비교 분석 
model_list <- list(GBM = gbmFit,
                   RF200 = rfFit200,
                   RF400 = rfFit400)

results <- resamples(model_list)

bwplot(results, layout = c(3, 1))
summary(results)

# Test Set 적용 결과 분석  
model_comparison <- sapply(model_list, function(md, testX, testY){
  pred <- predict(md, testX)
  cf <- confusionMatrix(pred, testY)
  
  cat(md$method, "----------------\n\n")
  print(cbind(cf$table, prop.table(cf$table,2)))
  
  cat("\n")
  
  return(cf$overall[1])
  
}, testX_P, testY)

model_comparison

max_ind <- which.max(model_comparison)
bestModel <- model_list[[max_ind]]

# ** 결과 저장하기   ------------------------------------------

save(bestModel, model_comparison, 
     file = paste0(MODEL_PATH, gsub(" |:", "_", Sys.time()), ".Rdata"))
load("MODEL/2018-10-16_14_01_47.Rdata")


# ' -----------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------
# > 5. 사후 분석  ------------------------ ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++------

# ** 확률 구간에 따른 정확도 및 LIFT 분석  ------------------------------------------
prob <- predict(bestModel, testX_P, type = "prob")
head(prob)

prob_cut <- cut(prob[,2], 
                breaks = seq(0,1,by=0.1),
                labels = paste0("rank-", 1:10))
head(prob_cut)

prob_df <- data.frame(probability = prob[,2],
                      rank = prob_cut,
                      true = testY)
head(prob_df)

meanSurvived <- mean(ifelse(testY=="1", 1, 0), na.rm=T)
meanSurvived

lift_table <-
  prob_df %>% group_by(rank) %>% 
  summarise(
    N = n(),
    N_Survived = sum(ifelse(true=="1", 1, 0), na.rm=T),
    probability = N_Survived / N) %>% 
  arrange(desc(rank)) %>% 
  mutate(cumN = cumsum(N),
         cumSurvived = cumsum(N_Survived),
         cumProb = cumSurvived / cumN,
         random_Prob = meanSurvived) %>% 
  mutate(lift = probability / random_Prob,
         cumLift = cumProb / random_Prob)

printResult("Probabilty & Lift Analysis ", lift_table)

lift_df <- data.frame(prob = seq(10, 100, by=10),
                      cumLift = lift_table$cumLift)

ggplot(lift_df, aes(prob, cumLift)) + 
  geom_line(aes(prob, 1), size=2) +
  geom_line(aes(prob, 2.6), size=2) +
  geom_line(size = 2, color ="red") +
  geom_point(size=4, shape=21, fill="white", color="red") + 
  ylim(c(0,3)) + ggtitle("Cumulative Lift Chart")

ggsave(paste0(MODEL_PATH, "LiftChart_", gsub(" |:", "_", Sys.time()), ".png"),
       dpi = 300, device = "png")

# ** ROC Chart ------------------------------------------

result_roc <- roc(testY, prob[,2])
printResult("AUC of Best Model ", result_roc$auc)


# ROC Curve
# ?plot.roc
plot(result_roc, print.thres="best", print.thres.best.method="closest.topleft")

plot(result_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

# Threshold and Accuracy
result_coords <- coords(result_roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
printResult("Threshold and Accuracy ", result_coords)

# graphics.off()



