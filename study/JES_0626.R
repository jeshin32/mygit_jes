
# review ------------------------------------------------------------------

#리스트의 반환 : 변수명&값

# for loop

for (i in 1:10) {
  print(i)
}

for (i in 1:10) {
  if(i%%2==1) {
    print(i)
  }
}

for (i in 1:10) {
  if(i%%2==1) {
    print(i)
  }
  else {
    print(i*3)
  }
}

cum <- 0
for (i in 1:10) {
  cum <- cum + i
}
cum

cum <- 1
for (i in 1:10) {
  cum <- cum * i
}
cum

for (i in 2:9) {
  cat(i, "단 입니다.------------- \n\n")
  for (j in 1:9) {
    cat(i, "x", j, "=", i*j, "\n")
  }
  cat("\n")
}

# download, all file (stringr::str_pad)

stringr::str_pad(1, width=4, pad=0)

# function

myfunc_e <- function() {
  
  return()
}

mtfunc_e <- function(a,b,c) {
  out <- a^3 + b^3 + c^3
  return(out)
}

mtfunc_e <- function(a,b=0,c=0) {
  out <- a^3 + b^3 + c^3
  return(out)
}

mycumsum2 <- function(n) {
  cum=0 # 초기값
  for (i in 1:n) {
    cum <- cum + i
  }
  return(cum)
}
mycumsum2(5)

#ggplot2

library(ggplot2)

names(iris)
#[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"   

g <-ggplot(iris, aes(Sepal.Length, Petal.Length, color=Species)) + # aes안에 변수가 아닌 객체가 들어감 no ""
  geom_point(size=5, alpha=0.5) +
  facet_wrap(~Species, nrow=3)
g

#library(plotly)
#ggplotly(g)

plotly::ggplotly(g)
ggsave("data/myggplot.png", wedth=10)    # 맨 마지막 그림을 저장

ggplot(iris, aes_string("Sepal.Length","Sepal.Width",color="Species")) +
  geom_point(size=5, alpha=0.5)

gl <- list()
k=1
for(var in names(iris)) {
  g <- ggplot(iris, aes_string("Sepal.Length", var, color="Species")) +
    geom_point(size=5, alpha=0.5)
  print(g)
  
  gl[[k]] <- g
  k = k+1
}

install.packages('gridExtra')
library(gridExtra)

grid.arrange(
  grobs= gl,
  widths = c(1,1,1),
  layout_matrix = rbind(c(1,2,3),
                        c(4,5,NA))
)

?grid.arrange



# SQL ---------------------------------------------------------------------

install.packages("sqldf")
library(sqldf)
data <- iris
sqldf('select * from data')
?sqldf


# tidyr -------------------------------------------------------------------
# gather, spread

data <- read.csv("data/widde_data.csv")
head(data)

library(tidyr)
gather(data, mon, sales, 2:13)

long <- gather(data, mon, sales, -업종)

ggplot(long, aes(mon, sales, fill=업종)) + geom_bar(stat="identity", position="fill") +
  theme(axis.text.x = element_text(angle=25,hjust = 1))

head(iris)
iris_long <- gather(iris, var, value, 1:4)
head(iris_long)

ggplot(iris_long, aes(var, value)) + geom_boxplot()

data <- read.csv("data/climate.csv")
head(data)

long_cli <- gather(data, var, value, 3:6)
head(long_cli)

ggplot(long_cli, aes(Year, value, color=var)) + geom_line() +
  facet_wrap(~var)


ggplot(long_cli, aes(Year, value, color=var)) + geom_bar(stat="identity") +
  facet_wrap(~var)


# wordcloud ---------------------------------------------------------------


install.packages('wordcloud2')
library(wordcloud2)  

wordcloud2(demoFreq)
wordcloud2(demoFreq[1:100,], shape = "star", size = 0.7, color = "skyblue")


# reticulate --------------------------------------------------------------

install.packages('reticulate')
library(reticulate)

py_run_string('from keras import layer')


# dplyr -------------------------------------------------------------------

install.packages('dplyr')
library(dplyr)

# select, filter, mutate, group_by, summarise, arrange

select(iris, 1,3,5)
select(iris, 1,3:5)
select(iris, -2)
select(iris, -Species)

filter(iris, Sepal.Length > 7 )
filter(iris, Sepal.Length > 7, Petal.Length > 6)
filter(iris, Sepal.Length > 7 & Petal.Length > 6)

#iris[iris$Sepal.Length>7,]

mutate(iris, nSL = Sepal.Length*100)
mutate(iris, nSL = Sepal.Length*100,
             nPL = Petal.Length*100)
mutate(iris, size=ifelse(Sepal.Length > 5, "big", "small"))

group_by(iris, Species)

summarise(iris, mSL = mean(Sepal.Length),
                mPL = mean(Petal.Length)) # sd, var

g <- group_by(iris, Species)
summarise(g, mSL = mean(Sepal.Length),
             mPL = mean(Petal.Length))

arrange(iris, Species, -Sepal.Length)
arrange(iris, desc(Species), -Sepal.Length)


# Chain operator ----------------------------------------------------------

# ctrl + shift + m     %>% 

iris %>% head() # == head(iris)   iris의 결과를 첫번째 함수의 파라미터로 보내겠다  
iris %>% head() %>% summary() # == summary(head(iris))


# Example -----------------------------------------------------------------

# iris data에서 1,3,5 데이터만 선택하여
# Species가 "setosa"가 아닌 애들로 한정하고
# Sepal.Length > 6, "big" or "small" 인 size 변수 만들어서
# size와 Species 별로
# Sepal.Length와 Petal.Length의 평균을 구하시오

iris %>% select(1,3,Species) %>%  # 5 == Species
  filter(Species != "setosa") %>%  # or를 먼저 인지함 and == ,
  mutate(size=ifelse(Sepal.Length > 6,"big","small")) %>%
  group_by(size, Species) %>% 
  summarise(mSL = mean(Sepal.Length),
            mPL = mean(Petal.Length),
            N = n()) %>%  # counting numbers of row
  arrange(size, -mSL) %>% 
  head()

iris %>% select(1,3,5) %>% 
  filter(Species != "setosa") %>% 
  ggplot(aes(Sepal.Length, Petal.Length, color=Species)) + geom_point(size=5)

library(tidyr)

iris %>% gather(var,value,1:4) %>% 
  ggplot(aes(var, value)) + geom_boxplot()

df <- data.frame(a=c(1,1,1,2,2),
                 b=c(1,1,2,1,1))
df
unique(df)

iris %>% select(1,3,Species) %>%  # 5 == Species
  filter(Species != "setosa") %>%  # or를 먼저 인지함 and == ,
  mutate(size=ifelse(Sepal.Length > 6,"big","small")) %>%
  group_by(size, Species) %>% 
  summarise(mSL = mean(Sepal.Length),
            mPL = mean(Petal.Length),
            N = n(),
            distN = n_distinct(Petal.Length, Sepal.Length)) %>%  # counting numbers of row
  arrange(size, -mSL) %>% 
  head()

iris %>% group_by_all() %>% 
  summarise(distN = n())

df %>%  group_by_all() %>% 
  summarise(distN = n())


# randomForest ------------------------------------------------------------

install.packages('randomForest')
library(randomForest)

md <- randomForest(Species ~ . , data=iris,
                   ntree=200)
md

new <- data.frame(
  Sepal.Length = 5, 
  Sepal.Width = 3,
  Petal.Length = 1,
  Petal.Width = 0.2
)
new
predict(md, new)

new2 <- data.frame(
  Sepal.Length = 6, 
  Sepal.Width = 3,
  Petal.Length = 5,
  Petal.Width = 2
)
predict(md, new2)
predict(md, new2, type = "prob")


new3 <- data.frame(
  Sepal.Length = 6, 
  Sepal.Width = 2,
  Petal.Length = 4.5,
  Petal.Width = 1.9
)
predict(md, new3)
predict(md, new3, type = "prob")

md$importance  # list

predict(md, iris[,-5])
md$predicted

rmd <- randomForest(Petal.Length ~ Petal.Width, # regression
                    data=iris,
                    ntree=200)
rmd

pred = predict(rmd, iris[,-3])
cbind(iris$Petal.Length, pred)


# keras -------------------------------------------------------------------

install.packages('devtools')
devtools::install_github("rstudio/keras")
library(keras)
install_keras()

# mnist -------------------------------------------------------------------

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test)



# ML in r -----------------------------------------------------------------

install.packages('rpart')
install.packages('kernlab')
install.packages('ellipse')
install.packages('e1071')
install.packages('caret')

library(caret)

data <- iris

#data split

idx <- sample(1:150, nrow(data)*0.7) #, replace=T) # random sampling
idx

train <- data[idx,]
test  <- data[-idx,]
dim(train)
dim(test)

#EDA

#Modeling

# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

fit.lda <- train( Species ~ . ,
                  data=train,
                  method='lda',
                  metric=metric,
                  trControl=control)
fit.lda

fit.knn <- train( Species ~ . ,
                  data=train,
                  method='knn',
                  metric=metric,
                  trControl=control)
fit.knn

fit.rf <- train( Species ~ . ,
                  data=train,
                  method='rf',
                  metric=metric,
                  trControl=control)
fit.rf

fit.svm <- train( Species ~ . ,
                 data=train,
                 method='svmRadial',
                 metric=metric,
                 trControl=control)
fit.svm

#model comparison
results <- resamples(list(
  lda = fit.lda, 
  knn = fit.knn, 
  svm = fit.svm, 
  rf = fit.rf))

summary(results)
dotplot(results)

#para tunning
grid <- expand.grid(k=seq(1,50,by=2))
grid

fit.knn2 <- train( Species ~ . ,
                  data=train,
                  method='knn',
                  tuneGrid = grid,
                  metric=metric,
                  trControl=control)
fit.knn2

plot(fit.knn2)


# predict & model performance ---------------------------------------------

pred <- predict(fit.knn2, test)
pred

confusionMatrix(pred, test$Species) # 예측값, 참값
