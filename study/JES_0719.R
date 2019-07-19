library(ggplot2)
library(dplyr)
library(stringr)
library(h2o)

set.seed(7)
out <- sample(1:9, 10000, replace=T)

table(out) %>% as.data.frame() %>% ggplot(aes(reorder(out,-Freq), Freq)) + geom_bar(stat="identity", fill="orange")
                                          
                                          
# h2o : RF, GBM, XGBoost, DL >> W2V, Autoencoder


# fill NA -----------------------------------------------------------------


df <- data.frame(
  income = c(100,110,120,400,NA,500,NA,700),
  area = c("A","A","A","B","B","B","C","C")
)
df

df %>% group_by(area) %>% summarise(mean=mean(income, na.rm=T))
fillna_mean <- df %>% summarise(mean=mean(income,na.rm=T))
df[is.na(df)] <- fillna_mean          
df

df %>% mutate(income=ifelse(is.na(income), mean(income,na.rm=T),income))                      
df %>% group_by(area) %>% mutate(income=ifelse(is.na(income), mean(income,na.rm=T),income))                      

# # Base R: 
# baseR.sbst.rssgn   <- function(x) { x[is.na(x)] <- 0; x }
# baseR.replace      <- function(x) { replace(x, is.na(x), 0) }
# baseR.for          <- function(x) { for(j in 1:ncol(x))
#   x[[j]][is.na(x[[j]])] = 0 }
# 
# # tidyverse
# ## dplyr
# dplyr_if_else      <- function(x) { mutate_all(x, ~if_else(is.na(.), 0, .)) }
# dplyr_coalesce     <- function(x) { mutate_all(x, ~coalesce(., 0)) }
# 
# ## tidyr
# tidyr_replace_na   <- function(x) { replace_na(x, as.list(setNames(rep(0, 10), as.list(c(paste0("var", 1:10)))))) }
# 
# ## hybrid 
# hybrd.ifelse     <- function(x) { mutate_all(x, ~ifelse(is.na(.), 0, .)) }
# hybrd.replace_na <- function(x) { mutate_all(x, ~replace_na(., 0)) }
# hybrd.replace    <- function(x) { mutate_all(x, ~replace(., is.na(.), 0)) }
# hybrd.rplc_at.idx<- function(x) { mutate_at(x, c(1:10), ~replace(., is.na(.), 0)) }
# hybrd.rplc_at.nse<- function(x) { mutate_at(x, vars(var1:var10), ~replace(., is.na(.), 0)) }
# hybrd.rplc_at.stw<- function(x) { mutate_at(x, vars(starts_with("var")), ~replace(., is.na(.), 0)) }
# hybrd.rplc_at.ctn<- function(x) { mutate_at(x, vars(contains("var")), ~replace(., is.na(.), 0)) }
# hybrd.rplc_at.mtc<- function(x) { mutate_at(x, vars(matches("\\d+")), ~replace(., is.na(.), 0)) }
# hybrd.rplc_if    <- function(x) { mutate_if(x, is.numeric, ~replace(., is.na(.), 0)) }
# 
# # data.table   
# library(data.table)
# DT.for.set.nms   <- function(x) { for (j in names(x))
#   set(x,which(is.na(x[[j]])),j,0) }
# DT.for.set.sqln  <- function(x) { for (j in seq_len(ncol(x)))
#   set(x,which(is.na(x[[j]])),j,0) }
# DT.fnafill       <- function(x) { fnafill(df, fill=0)}
# DT.setnafill     <- function(x) { setnafill(df, fill=0)}


# h2o autoencoder ---------------------------------------------------------

#  Pretrained autoencoder를 모델에 활용하기
#  #오토인코더를 먼저 학습시키고 중간에 차원 축소가 된 레이어를 빼내서 후단 모델의 input data로 넣는다

library(h2o)
h2o.init(nthreads = -1)
  
data <- h2o.importFile("C:/Users/Uxxxxxxx/Downloads/HD - 201906/Data/wine.csv")
head(data)

y <- "Cvs"
x <- setdiff(names(data), y)

data[,y] <- as.factor(data[,y])

splits <- h2o.splitFrame(data, 0.7, seed=1)
train <- splits[[1]]
test <- splits[[2]]

# auto encodeing

hidden <- c(6,3,6)

ae_model <- h2o.deeplearning(x = x, 
                             training_frame = train,
                             model_id = "wine_autoencoder",
                             ignore_const_cols = FALSE,
                             activation = "Tanh",  # Tanh is good for autoencoding
                             hidden = hidden,
                             autoencoder = TRUE)
ae_model


# Deep Features -----------------------------------------------------------

# convert train_supervised with autoencoder model to lower-dimensional space
train_reduced_x <- h2o.deepfeatures(ae_model, train, layer = 1)
dim(train_reduced_x) 
train_reduced <- h2o.cbind(train_reduced_x, train[,y])

test_reduced_x <- h2o.deepfeatures(ae_model, test, layer = 1)
test_reduced <- h2o.cbind(test_reduced_x, test[,y])

# use x
rf1 <- h2o.randomForest(x=x, y=y,
                        training_frame = train,
                        ntrees=100, seed=1)
rf1

rf1_perf <- h2o.performance(rf1, newdata = test)
h2o.mse(rf1_perf)

# use ae
rf2 <- h2o.randomForest(x = names(train_reduced_x), y = y, 
                        training_frame = train_reduced,
                        ntrees=100, seed=1)

rf2_perf <- h2o.performance(rf2, newdata = test_reduced)
h2o.mse(rf2_perf)

fit1 <- h2o.deeplearning(x=x,y=y,
                         training_frame=train,
                         ignore_const_cols = F,
                         hidden=hidden,
                         epochs=1000,
                         pretrained_autoencoder = "wine_autoencoder")
fit1_pref <- h2o.performance(fit1, newdata=test)
fit1_pref


# Heart.csv ---------------------------------------------------------------

library(h2o)
h2o.init(nthreads = -1)

data <- h2o.importFile("C:/Users/Uxxxxxxx/Downloads/LECTURE/HD - 201906/Data/Heart.csv")[,-(1:2)]
head(data)

y <- "AHD"
x <- setdiff(names(data), y)

data[,y] <- as.factor(data[,y])

splits <- h2o.splitFrame(data, 0.7, seed=1)
train <- splits[[1]]
test <- splits[[2]]

# auto encodeing

hidden <- c(6,3,6)

ae_model <- h2o.deeplearning(x = x, 
                             training_frame = train,
                             model_id = "heart_autoencoder",
                             ignore_const_cols = FALSE,
                             activation = "Tanh",  # Tanh is good for autoencoding
                             hidden = hidden,
                             autoencoder = TRUE)
ae_model

# Deep Features -----------------------------------------------------------

# convert train_supervised with autoencoder model to lower-dimensional space
train_reduced_x <- h2o.deepfeatures(ae_model, train, layer = 1)
dim(train_reduced_x) 
train_reduced <- h2o.cbind(train_reduced_x, train[,y])

test_reduced_x <- h2o.deepfeatures(ae_model, test, layer = 1)
test_reduced <- h2o.cbind(test_reduced_x, test[,y])

# use x
rf1 <- h2o.randomForest(x=x, y=y,
                        training_frame = train,
                        ntrees=100, seed=1)
rf1

rf1_perf <- h2o.performance(rf1, newdata = test)
h2o.mse(rf1_perf)

# use ae
rf2 <- h2o.randomForest(x = names(train_reduced_x), y = y, 
                        training_frame = train_reduced,
                        ntrees=100, seed=1)

rf2_perf <- h2o.performance(rf2, newdata = test_reduced)
h2o.mse(rf2_perf)

fit1 <- h2o.deeplearning(x=x,y=y,
                         training_frame=train,
                         ignore_const_cols = F,
                         hidden=hidden,
                         epochs=1000,
                         pretrained_autoencoder = "heart_autoencoder")
fit1_pref <- h2o.performance(fit1, newdata=test)
fit1_pref

# Anomaly Detection -------------------------------------------------------

test_rec_error <- as.data.frame(h2o.anomaly(ae_model, test)) 
test_recon <- predict(ae_model, test)

dim(test_rec_error)
dim(test_recon)
head(test_recon)


# heart for coffee --------------------------------------------------------

data <- h2o.importFile("C:/Users/Uxxxxxxx/Downloads/LECTURE/HD - 201906/Data/Heart.csv")[,-(1:2)]
head(data)

y <- "AHD"
x <- setdiff(names(data), y)

data[,y] <- as.factor(data[,y])

splits <- h2o.splitFrame(
  data = data, 
  ratios = c(0.5,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

dim(train)
dim(valid)
dim(test)

# Establish baseline performance ---------------------------------------

gbm <- h2o.gbm(x = x, y = y, training_frame = train)
gbm

h2o.auc(h2o.performance(gbm, newdata = valid)) 


# grid search -------------------------------------------------------------
?h2o.grid

hyper_params = list( max_depth = seq(1,29,2) )

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_grid",
  x = x, 
  y = y, 
  training_frame = train, 
  validation_frame = valid,
  ntrees = 100000,                                                            
  learn_rate = 0.01,                                                         
  learn_rate_annealing = 0.99,                                               
  sample_rate = 0.8,                                                       
  col_sample_rate = 0.8, 
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  score_tree_interval = 10                                                
)

grid                                                                       
## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid


## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth

best_gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
best_gbm@parameters

grid2 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="randomForest",
  grid_id="depth_grid2",
  x = x, 
  y = y, 
  training_frame = train, 
  validation_frame = valid,
  ntrees = 100000,                                                          
  sample_rate = 0.8,                                                       
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  score_tree_interval = 10                                                
)
grid2                                                                      
## sort the grid models by decreasing AUC
sortedGrid2 <- h2o.getGrid("depth_grid2", sort_by="auc", decreasing = TRUE)    
sortedGrid2

best_rf <- h2o.getModel(sortedGrid2@model_ids[[1]])
best_rf@parameters

# Model Inspection and Final Test Set Scoring -----------------------------

my_rf <- h2o.getModel(sortedGrid2@model_ids[[1]])
print(h2o.auc(h2o.performance(my_rf, newdata = test)))

# hands -------------------------------------------------------------------

my_gbm <- h2o.gbm(x = x, 
               y = y, 
               training_frame = h2o.rbind(train, valid), 
               nfolds = 4, 
               ntrees = 10000,
               learn_rate=0.05, 
               learn_rate_annealing = 0.99,
               max_depth=13,
               distribution="bernoulli",
               stopping_rounds = 5, 
               stopping_tolerance = 1e-4,
               stopping_metric = "AUC",
               sample_rate = 0.8,                                                       
               col_sample_rate = 0.8,                                                   
               seed = 1234,                                                             
               score_tree_interval = 10)
my_gbm@model$cross_validation_metrics_summary
h2o.auc(h2o.performance(my_gbm, newdata = valid)) 
h2o.auc(h2o.performance(my_gbm, newdata = test)) 

gbm <- h2o.performance(my_gbm, newdata = test)
gbm

deep1 <- h2o.deeplearning(x = x, y = y,
                          training_frame = h2o.rbind(train, valid),
                          nfolds = 4,
                          ignore_const_cols = FALSE,
                          hidden = c(5,2),
                          stopping_rounds = 5,
                          stopping_tolerance = 1e-4,
                          stopping_metric = "MSE",
                          epochs=1000000,
                          mini_batch_size=5)
perf1 <- h2o.performance(deep1, newdata = test)
perf1
# 
# 
# ?h2o.deeplearning

# Ensembling Techniques ------------------------------------------------2

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)


deep_perf <- h2o.predict(deep1, newdata = test)
deep_perf
gbm_perf <- h2o.predict(my_gbm, newdata = test)
gbm_perf

ensemble = (deep_perf[,2]+gbm_perf[,2])/2


ensemble$Yes <- (1 - ensemble$No)

ensemble <- ensemble %>% as.data.frame() %>%  mutate(predict=if_else(No > 0.5, "No", "Yes"))
answer <- as.vector(test[,y])
answer
ensemble$answer <- answer

1 - 13/86

# power mean --------------------------------------------------------------

pmean <- function(x, p) {
  library(TTR)
  return(tail(TTR::SMA(x^p, length(x))^(1/p), n=1))
}


# autoML ------------------------------------------------------------------

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)


# The leader model is stored here
aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works

# or:
pred <- h2o.predict(aml@leader, test)
