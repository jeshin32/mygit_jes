#rm(list=ls())

# review 1st week ---------------------------------------------------------

#matrix function
#dim(mat)
#nrow(mat)
#ncol(mat)

#[] >> 리스트의 부분집합 : 결과도 리스트


#리스트 지우기 assign >> NULL

dim(iris)
head(iris)

iris[iris$Sepal.Length < 7 , "Sepal.Length"]
#iris[iris$Sepal.Length < 7 , Sepal.Length] >> error

#libaray
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

ggplot(data,aes()) + geom_point()

library(gridExtra)

gl <- list()
k=1
for(var in names(iris)) {
  g <- ggplot(iris, aes_string("Sepal.Length", var, color="Species")) +
    geom_point(size=5, alpha=0.5)
  print(g)
  
  gl[[k]] <- g
  k = k+1
}

grid.arrange(
  grobs= gl,
  widths = c(1,1,1,1),
  layout_matrix = rbind(c(1,2,3,3),
                        c(4,5,NA,NA))
)


# skt delivery data -------------------------------------------------------

data <- read.csv("data/SKT_DELIVERY.csv")

# 1) understanding data

dim(data)
head(data)
str(data) #time CHANGE to factor, day LEVEL change

# 2) data preprocessing

#type <- 분류문제와 회귀문제 구분을 위해 y값의 내용을 지정
data$시간대 <- as.factor(data$시간대)
levels(data$요일) <- c('월','화','수','목','금','토','일')
data$일자 <- as.factor(data$일자)

str(data)

# 3) EDA(Exploratory Data Analysis)

summary(data) #범위를 봐야한다

#change levels of factors
levels(data$요일)
levels(data$일자)
levels(data$시군구)

#check levels
for (i in 1:8) {
  
  if(is.factor(data[,i])) { 

    cat(names(data[i]),"--------------\n\n")
    lv <- levels(data[,i])
    print(lv)
    cat("\n\n")
  }
}

#가설을 세우고 데이터를 요약해 본다

#시군구별 통화건수 구하기

d1 <- data %>% group_by(시군구) %>% summarise(sum_call = sum(통화건수)) %>% 
  arrange(-sum_call)
d1

g1 <- d1 %>% ggplot(aes(reorder(시군구, -sum_call), sum_call)) +
  geom_bar(stat='identity', fill = 'skyblue') +
  theme(axis.text.x = element_text(angle=45))
g1


d2 <- data %>% group_by(요일) %>% summarise(sum_call = sum(통화건수)) %>% 
  arrange(-sum_call)

g2 <- d2 %>% ggplot(aes(요일, sum_call)) +
  geom_bar(stat='identity', fill = 'skyblue') +
  theme(axis.text.x = element_text(angle=45))
g2

for (var in names(data)[1:7]) {
  
  out <-
  data %>% group_by_(var) %>%
    summarise(sum_call = sum(통화건수)) %>% 
    arrange(-sum_call)
  g <-
    out %>% ggplot(aes_string(var, "sum_call")) +
    geom_bar(stat="identity", fill="skyblue")
    
  print(g)
} 

#이변량/다변량 분석 -- 가설&검증

# 가설1. 각 요일별로 업종 특성이 다른가?
out <-
data %>%  group_by(요일,업종) %>% 
  summarise(sum_call = sum(통화건수)) %>% 
  arrange(요일, -sum_call)

# 구성비 그래프
out %>% ggplot(aes(요일,sum_call,fill=업종)) +
                 geom_bar(stat="identity",position="fill")

# 가설2. 각 구별로 업종 특성이 다른가?
out <-
  data %>%  group_by(시군구,업종) %>% 
  summarise(sum_call = sum(통화건수)) %>% 
  arrange(시군구, -sum_call)

# 구성비 그래프
out %>% ggplot(aes(시군구,sum_call,fill=업종)) +
  geom_bar(stat="identity",position="fill") +
  theme(axis.text.x = element_text(angle=45))


# 구성비가 아닌 전체 값에 대해 그래프를 보게 되면 구성비의 특성은 알 수가 없다
out %>% spread(업종,sum_call)

iris %>% gather(var, value, -5) %>% 
  group_by(Species,var) %>% 
  summarise(mean=mean(value)) %>% 
  spread(var, mean)


# data.table --------------------------------------------------------------

install.packages('data.table')
library(data.table)
install.packages('Rtools')

df <- read.csv("data/requisites.csv", fileEncoding = "euc-kr")
class(df)

dt <- data.table(df)
class(dt)

#행추출
dt[1] # dt[1,]
dt[c(1,3,5)]

dt[1,] #행을 뽑는 거라고 명시적으로 표시하는게 좋습니다

#열추출
dt[,1]
dt[,c(1,3,5)]
dt[,M_NAME]
dt[,list(M_NAME, P_SEQ)]
dt[,.(M_NAME, P_SEQ)] # ==dt[,list(M_NAME, P_SEQ)]

#by, mutate, summarise
dt[,mean(A_PRICE), by=M_TYPE_NAME] 
dt[,mean(A_PRICE), by='M_TYPE_NAME']  # ==dt %>% group_by(M_TYPE_NAME) %>% summarise(mean = mean(A_PRICE))

dt[,mean(A_PRICE), by=list(M_GU_NAME,M_TYPE_NAME)] 

dt[,mean(A_PRICE), by=.(M_GU_NAME,M_TYPE_NAME)] %>% 
  spread(M_GU_NAME, V1)

#filter
dt[M_GU_NAME=='영등포구',]
setkey(dt, M_GU_NAME) #indexing을 해둔다 빨리 찾기 위해(making dictionary key)

dt['영등포구',]
dt[J('영등포구'),]


# titanic data ------------------------------------------------------------

titanic <- read.csv("Data/titanic.csv")
class(titanic)

dt <- fread("Data/titanic.csv")
class(dt)

system.time({
  df <- read.csv("data/mnist_train.csv")
})

system.time({
  df <- fread("data/mnist_train.csv")
})

View(dt)

# choice column
dt[,1]
dt[,pclass]
dt[,list(pclass,sex)]
dt[,.(pclass,sex)]

# filter
dt[pclass =="1st",]

setkey(dt,pclass)
tables()

dt["1st",]
dt[J("1st"),]

setkeyv(dt, c("sex","pclass")) # 두번째 index는 안 먹네...
tables()

dt["1st",] #error

?setkeyv

#survived rate
dt[, mean(survived)]
dim(dt)
dt[,sum(survived)]
dt[, mean(survived),by=pclass]

dt[, lapply(.SD, mean),
   by = sex,
   .SDcols = c("survived")]
#.SDcols : 연산에 필요한 컬럼을 정의한다(like select)
#.SD : group by된 data set

dt[, lapply(.SD, mean, na.rm=T),
   by = sex,
   .SDcols = c("survived","age")]


# lapply ------------------------------------------------------------------

lapply(iris, class) #class == list
#앞 df의 리스트 열마다 뒷 함수를 for loop 돌린 효과가 남
#class(iris[i])

sapply(iris, mean) #class == numeric
sapply(iris, class) #class == numeric

lapply(dt[,5:6], mean, na.rm=T)

# iris example

iris_dt <- data.table(iris)

iris_dt [, lapply(.SD, mean, na.rm=T),
         by = Species,
         .SDcols = names(iris)[1:4]]

setkey(dt,pclass)
dt[J("1st"), length(which(age > 20))/.N, by = "sex"] 


# .N ----------------------------------------------------------------------

dt[,sum(pclass=="1st")]
dt["1st", .N] #count rows ==n()

dt["1st", .N, by="sex"]
dt %>% filter(pclass=="1st") %>% 
  group_by(sex) %>% 
  summarise(n())

dt["1st", nrow(.SD), by="sex"]
dt["1st", ncol(.SD), by="sex"]
dt["1st", ncol(.SD), by="sex", .SDcols = c("age","ticket")]

dt[,.N,by="pclass"]

#which(c(1,2,3,4)>2)
#which.max(c(1,2,3,4))
#조건값으로 false/true 하면 0,1 이니까 sum 하면 비율을 알 수 있다
dt[J("1st"),
   length(which(age>20))/.N, by="sex"]

dt[J("1st"),
   sum(age>20,na.rm=T)/.N, by="sex"]

dt[J("1st"),
   sum(age>20,na.rm=T)/nrow(.SD), by="sex"]

dt[J("1st"),
   sum(age>20,na.rm=T), by="sex"]
dt[J("1st"),
   nrow(.SD), by="sex"]

#mutate
dt[, isminor :="adult"] # 원 데이터 변형됨
dt
dt[,":="(isminor = "adult",
         aaa = "kkk")]
dt

iris_dt[,size := ifelse(Sepal.Length>5,"big","small")] %>% 
  group_by(size) %>% 
  summarise(mSL = mean(Sepal.Length))

iris_dt[,size := ifelse(Sepal.Length>5,"big","small")][,mean(Sepal.Length), by=size]

iris_dt[,size := ifelse(Sepal.Length>5,"big","small")] %>% 
  .[,mean(Sepal.Length), by=size] %>% 
  arrange(V1)


# https://www.listendata.com/2016/10/r-data-table.html --------------------

mydata = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
#fwrite(mydata, "data/data_table_50example.csv")

nrow(mydata)
names(mydata)
View(mydata)

dat1 <- mydata[ , origin] # returns a vector
dat1
dat1 <- mydata[ , .(origin)] # returns a data.table
dat1
dat1 <- mydata[, c("origin"), with=FALSE]
dat1

mydata[, -c("origin")]
#mydata[, c("origin"), with=FALSE]

dat6 <- mydata[, !c("origin", "year", "month"), with=FALSE]
head(dat6)

dat7 <- mydata[,names(mydata) %like% "dep", with=FALSE]
head(dat7)

mydata[origin %in% c("JFK", "LGA")]
mydata[!origin %in% c("JFK", "LGA")]

mydata_C <- copy(mydata)
mydata_C[, c("dep_sch","arr_sch"):=list(dep_time - dep_delay, arr_time - arr_delay)]

View(mydata_C)
