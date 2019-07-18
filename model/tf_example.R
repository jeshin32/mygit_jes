library(tensorflow)
library(Matrix)
library(caret)
library(pROC)
library(data.table)

tf$set_random_seed(1)

data(Sonar, package="mlbench")

Sonar[,61] = as.numeric(Sonar[,61])-1

#scale ,centering으로 변수 전처리 수행 on caret
sonar_scaled <- predict(preProcess(Sonar[,-61]), Sonar[,-61])

#학습셋과 테스트셋 분류 
train.ind<- createDataPartition(Sonar[,61],p = 0.7, list = F)

train.x <- data.matrix(sonar_scaled[train.ind[,1],])
train.y <- Sonar[train.ind, 61]
test.x <- data.matrix(sonar_scaled[-train.ind[,1],])
test.y <- Sonar[-train.ind, 61]


num_x <- as.integer(ncol(train.x))
num_y <- 1L

x <- tf$placeholder(dtype = tf$float32, shape = list(NULL, num_x))
y <- tf$placeholder(dtype = tf$float32, shape = list(NULL, num_y))

# #일반적인 
# with(tf$name_scope("DNN"),{ 
#   fc1 <- tf$contrib$layers$fully_connected(x,10L,
#                                            activation_fn=tf$nn$relu, weights_initializer=tf$contrib$layers$xavier_initializer(uniform = F))
#   pred <- tf$contrib$layers$fully_connected(fc1, num_y, 
#                                             weights_initializer=tf$contrib$layers$xavier_initializer(uniform = F))
# })

# 레이어 추가
with(tf$name_scope("DNN"),{ 
  fc1 <- tf$contrib$layers$fully_connected(x,10L,
                                           activation_fn=tf$nn$relu, weights_initializer=tf$contrib$layers$xavier_initializer(uniform = F))
  fc2 <- tf$contrib$layers$fully_connected(fc1,10L,
                                           activation_fn=tf$nn$relu, weights_initializer=tf$contrib$layers$xavier_initializer(uniform = F))
  pred <- tf$contrib$layers$fully_connected(fc2, num_y, 
                                            weights_initializer=tf$contrib$layers$xavier_initializer(uniform = F))
})

#노드 1개 출력이니 binary cross entropy로 계산한다.
#AdaGrad와 Momentum을 융합한것과 같은 효과를 보이는 AdamOptimizer를 사용한다. 
#GPU가 있다면 아래 CPU를 GPU로 바꿔주면 된다. 
with({tf$name_scope("loss");tf$device('/cpu:0')},{
  loss <- tf$reduce_mean(tf$nn$sigmoid_cross_entropy_with_logits(pred, y))
  optimizer <- tf$train$AdamOptimizer(learning_rate=0.01)$minimize(loss)
})

#Accuracy계산 
#ROC계산은 Tensorflow에서 다소 불편하니 R에서 별도로 계산할 것이다. 
with(tf$name_scope("metric"), {
  compare_pred <- tf$cast(tf$sigmoid(pred) > 0.5, tf$float32)
  accuracy <- tf$reduce_mean(tf$cast(tf$equal(compare_pred, y),tf$float32))
})



# training 
num_of_epoc <- 100L


sess <- tf$Session()

sess$run(tf$global_variables_initializer())

n_batch <- 5

aucs <- list()
for(i in 1:num_of_epoc){
  #print(sprintf("epoc %d", i))
  
  folds <- createFolds(train.y, k = n_batch)
  for(j in 1:n_batch){
    sess$run(optimizer, dict(x=train.x[folds[[j]],], y=matrix(train.y[folds[[j]]])))
  }
  
  accu <- sess$run(tf$sigmoid(pred), dict(x=test.x))
  accur <- sess$run(accuracy, dict(x=test.x, y=matrix(test.y)))
  
  aucs[[i]]<- data.table(epoc=i,aucs=as.numeric(auc(roc(test.y, accu[,1]))),  accuracy=accur)
}

dt_aucs <- rbindlist(aucs)

knitr::kable(dt_aucs[order(-aucs)][1:5])

# http://freesearch.pe.kr/archives/4546