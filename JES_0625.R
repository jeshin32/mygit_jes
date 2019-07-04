

# review ------------------------------------------------------------------


rm(list = ls()) #모든 변수 제거
View(iris) # 첫글자 대문자

x <- c(1,1,1,1,1)
y <- c(3,4)
c <- x+y
c

seq(1,length=10,by=0.5)
x <- 3:5
rep(x,4)
rep(x,each=4)

paste("x",1:10)
paste0("x",1:10)

idx = rep(c("x","y","z"), each=10)
paste0(idx,1:10)

v <- 1:30
dim(v) <- c(6,5)
v

mat <- matrix(1:30, nrow=6)
mat <- matrix(1:30, nrow=6, byrow = T)
mat

dim(mat)
dim(mat)[1]

lst <- list(
  name="jes",
  age=30,
  cert=LETTERS[1:5],
  mat=matrix(1:6, nrow=3)
)
list
lst

lst[3] # list의 부분집합 : 속성이 변하지 않는다 : 결과도 list
class(lst[1])
length(lst[3])
lst[3][3] # error
lst[[3]][3] # == lst$cert[3]
length(lst[[3]])

lst$new <- 1:10
lst
lst$new <- NULL
lst


df <- data.frame(
  A = 1:5,
  B = 5:1,
  C = LETTERS[3:7]
)
df
df$A
df[1] # df의 부분집합
df[[1]][3]
class(df[1:2])
length(df[1:2])

df[2,]
df[,2:3] #문법이 matrix와 같다

str(iris)
summary(iris) # find outlier

iris[iris$Sepal.Length>7,]
iris[iris$Sepal.Length>7,-5] # Species제외


# for loop ----------------------------------------------------------------

for(i in 1:10) {
  print(i)
}

for(i in c("orange","banana")) {
  print(i)
}

for(i in names(iris)) {
  print(i)
}

for(i in seq(1,50, by=4)) {
  print(i)
}

for(i in 1:10) {
  cat(i, "입니다.\n")
}


for(i in 1:10) {
  if(i %% 2  == 1) {
    cat(i, "는 홀수입니다.\n")
  } else {
    cat(i, "는 짝수입니다.\n")
  }
}

# gugudan
for(i in 2:9) {
  cat(i, "단입니다.-----\n\n")
  #i=i
  for(j in 1:9) {
    cat(i, "x", j, "=", i*j,"\n")
  }
  cat("\n")
}


# 1부터 100000까지의 합을 계산하시오 --------------------------------------------------

cum <- 0
for(i in 1:100000) {
  cum <- cum + i
}
cum


# 1부터 100까지의 누적곱을 계산하시오 ---------------------------------------------------

cum <- 1
for(i in 1:100) {
  cum <- cum * i
}
cum


# function ----------------------------------------------------------------

myfn <- function() {
  
  return()
} # basic

myfn <- function(a,b,c) {
  out <- a^2 + b^2 + c^2
  return(out)
}
myfn(1,2,3)
myfn(177,28,3)
myfn(1,2) # error

myfn2 <- function(a=0,b=0,c=0) {
  out <- a^2 + b^2 +c^2
  return(out)
}
myfn2(1,2) # not error
myfn2()


# 누적합 ---------------------------------------------------------------------

ex1 <- function(a) {
  cum <- 0
  for(i in 1:a) {
    cum <- cum+i
  }
  return(cum)
}
ex1(10)


# 누적곱 ---------------------------------------------------------------------

ex2 <- function(a) {
  cum <- 1
  for(i in 1:a) {
    cum <- cum*i
  }
  return(cum) # 안 써주면 최종 계산 값만을 반환해준다
}
ex2(10)


# 사칙연산 반환 함수 --------------------------------------------------------------

ex3 <- function(a=0,b=0) {
  x1=a+b
  x2=a-b
  x3=a*b
  x4=a/b
  print(x1)
  print(x2)
  print(x3)
  print(x4)
}

#answer
my4calc <- function(a,b) {
  lst <- list(
    sum = a+b,
    minus = a-b,
    prod = a*b,
    div = a/b
  )
  return(lst)
}
my4calc(1,2)

re <- my4calc(3,5)
re$sum


# source ------------------------------------------------------------------

source("Data/myfunctions.R")

mycumprod(10)
mycumsum(10)
my4calc(5,7)



# downloads ---------------------------------------------------------------

url = "http://www.iexceller.com/MyXls/Lectures/VisualBasic/VB0001.zip"
savefile = "data/excel_bbb.zip"

download.file(url,savefile)

library(stringr) 

str_pad(1,width=4,pad=0) # padding
str_pad(13,width=4,pad=0)
str_pad(13,width=4,pad=0,side="right")

for (i in 1:10) {
  url = paste0("http://www.iexceller.com/MyXls/Lectures/VisualBasic/VB",
               str_pad(i, width=4, pad=0),
               ".zip")
  savefile = paste0("Data/PVB",
                    str_pad(i, width=4, pad=0),
                    ".zip")
  download.file(url,savefile)
}

?download.file

for(i in 101:200) {
  url = paste0("http://www.iexceller.com/MyXls/Lectures/VisualBasic/VB",
               str_pad(i, width=4, pad=0),
               ".zip")
  savefile = paste0("Data/PVB", str_pad(i, width=4, pad=0), ".zip")
  
  download.file(url,savefile)
}



# excel handling ----------------------------------------------------------

install.packages('readxl')
library(readxl)

read_excel('data/ex.xlsx')
read_excel('data/ex.xlsx', sheet=2)
read_excel('data/ex.xlsx', sheet=3)

?read_excel

read_excel('data/ex.xlsx', range="shC!B6:E18")


# smartbind ---------------------------------------------------------------

install.packages('gtools')
library(gtools)
?smartbind

df1 <- data.frame(A=1:10, B=LETTERS[1:10], C=rnorm(10) )
df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )
df1
df2

smartbind(df1,df2)

all <- data.frame() # empty df
all
for(i in 1:3) {

  data <- read_excel("data/smartbind.xlsx", sheet = i)
  print(data)
  
  all <- smartbind(all, as.data.frame(data))
}

class(all)
all

write.csv(all, "data/all_bind.csv", row.names = F, na="")

install.packages('xlsx')
install.packages('rJava')

library(xlsx)
library(rJava)

write.xlsx(all,             # R데이터명
           file="data/smartbind.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new",  # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,   # 변수이름을 그대로 사용
           row.names=FALSE,  # 행이름은 사용하지 않음
           append=TRUE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장


# save --------------------------------------------------------------------

ls()
save(data, df1, df2, file="mydata.rdata")
rm(list=ls())
load("mydata.rdata")
ls()

myfunc <- function(n) {n^2}

save(data,df1,df2,myfunc, file="mydata.rdata")
rm(list=ls())
load("mydata.rdata")
ls()

myfunc(2)


# ggplot2 -----------------------------------------------------------------

library(ggplot2)

names(iris)
# "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"     
ggplot(iris,aes(Sepal.Length, Sepal.Width)) + geom_point()

g <- ggplot(iris,aes(Sepal.Length, Sepal.Width)) # canvas

g + geom_point()
g + geom_line()
g + geom_point() + geom_line()

ggplot(iris,aes(Sepal.Length, Sepal.Width)) + geom_point(size=5, alpha=0.5, color="red")

ggplot(iris,aes(Sepal.Length, Sepal.Width, color=Species, size=Sepal.Length)) +
  geom_point(alpha=0.5)
#변수와 연관된 내용은 전부 aes 안에 넣는다

ggplot(iris,aes(Sepal.Length, Sepal.Width, color=Species, size=Sepal.Length)) +
  geom_point(alpha=0.5) + facet_wrap(~Species, nrow=3) # Species로 차트 나눠 그리기

ggplot(iris,aes(Sepal.Length, Sepal.Width, color=Species, size=Sepal.Length, shape=Species)) +
  geom_point(alpha=0.5) + facet_wrap(~Species, nrow=3) + geom_smooth(method = "lm")

ggplot(iris, aes(Species, Sepal.Length, color=Species)) + geom_boxplot(outlier.size = 2)

ggplot(iris,aes(Petal.Length, fill=Species)) + geom_histogram(bins = 15, alpha=0.4) +
  facet_wrap(~Species, nrow=3)

ggplot(iris, aes(Species, Sepal.Length, color=Species)) + geom_boxplot(outlier.size = 2) +
  theme(axis.text.x = element_text(angle=25,hjust = 1))

# find = ctrl + shift + f

install.packages('plotly')
library(plotly)

g <- ggplot(iris,aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(alpha=0.5) 
ggplotly(g)

library(MASS)

head(birthwt)
str(birthwt)

birthwt$race <- as.factor(birthwt$race)
str(birthwt)

ggplot(birthwt, aes(factor(race),bwt)) + geom_boxplot()
ggplot(birthwt, aes(factor(smoke),bwt)) + geom_boxplot()

data <- read.csv("data/climate.csv")
head(data)

ggplot(data, aes(Year, Anomaly.5y)) + geom_line(color="blue") +
  geom_point(shape=21, color="blue", fill="white") +  # 21 = "o" 
  geom_line(aes(Year, Anomaly.1y), color="red")



# long data ---------------------------------------------------------------


library(tidyr)

long <- gather(data, var, value, 3:6)

ggplot(long, aes(Year, value, color= var)) + geom_line() +
  facet_wrap(~var)

wide <- read.csv("data/wide_data.csv")
head(wide)

long2 <- gather(wide, month, sales, 2:13) # 2:13 = -1 = -업종 (첫번째만 빼라)

head(long2)

ggplot(long2, aes(month, sales, fill = 업종)) + geom_bar(stat="identity")

spread(long2, month, sales)


# r_markdown ----------------------------------------------------------------

file -> newfile -> r markdown


