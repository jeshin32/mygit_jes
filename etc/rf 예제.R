
library(h2o)

set.seed(1)
df <- data.frame( x = c(runif(300), rep(1:8,2)),
                  y = c(runif(300), rep(1:8,2) + rnorm(16)*0.5))
te <- data.frame( x = seq(0,8, by=0.01))


library(ggplot2)
ggplot(df, aes(x,y)) + geom_point()


h2o.init()

md <- h2o.randomForest(x='x',y='y',
                       training_frame = as.h2o(df),
                       ntree=100, 
                       min_rows = 2)
md


library(dplyr)
re <- data.frame(
  x =te$x, 
  h2o.predict(md,as.h2o(te)) %>% as.data.frame()
)

re

ggplot(re, aes(x,predict)) + geom_point()

