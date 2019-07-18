

# review ------------------------------------------------------------------

# skt delivery ------------------------------------------------------------

# data load
# SETTING library
# data preprocessing - 변수 타입변화, 변수 변환
# EDA (Exploratory data analysis)
#  - 단변량 통계/차트
#  - 이변량 통계/차트
# 하나에 대해서 해보고 >> 자동화 하여 진행 / 마크다운 : 시사점



# data.table --------------------------------------------------------------

# 기본구조
#      dt[i,j,by=]
#        i = row, j = column & 표현식 by = group by,
# .SDcols =
# .SD
# .N
# shift()

dt <- data.table(
  A = 1:5,
  B = letters[1:5]
)

library(data.table)
library(dplyr)
library(tidyr)

dt
dt[, new := shift(A, n = 1, type = "lag")]
dt[, lead := shift(A, n = 2, type = "lead")]
dt


# how to use 'i' ----------------------------------------------------------

dt[2] # df에서는 2번째 열 (두번째 리스트)
dt[2,]
dt[1:2,]
dt[c(1,3,5),]

dt[A>3,] # df[df$A >3]

setkey(dt,B)
key(dt)
dt["e"] # key - fast
dt[J("e")] # key - faster
dt[B == "e"] # slow

dt$C <- LETTERS[1:5]
dt

setkeyv(dt, c("B","C"))
key(dt)
dt[J("e","E")]
dt[J("e")]



# how to use 'j' ----------------------------------------------------------
#  >> select, mutate, summarise

# select
dt[,A] # vector
dt[,"A"] # list
dt[,1] # list

dt[,c(B,C)] # vector
dt[,c("B","C")] # list - df grammar
dt[,list(B,C)] # list
dt[,.(B,C)] # list

# mutate
dt[,new2 := sample(1:3,5,replace = TRUE)]
dt

dt[,":="(n1 = 1,
         n2 = 2)] 
dt[,c("nn1","nn2") := .(3,4)] # *******
dt[,c("nn1","nn2") := list(3,4)]
dt


ff <- lapply(1:10, function(x) runif(6))
class(ff)

dt2 <- data.table()
dt2

dt2[,paste0("x", 1:10) := lapply(1:10, function(x) runif(6))]
dt2

dt2[,paste0("x", 1:10) := lapply(paste0("x", 1:10), function(x) dt2[,get(x)]*10) ] # get() 변수가 가지고 있는 객체(값)를 가지고 옴
dt2[,paste0("c", 1:10) := lapply(paste0("x", 1:10), function(x) dt2[,get(x)]*10) ] # get() 변수가 가지고 있는 객체(값)를 가지고 옴

dt2

# summarise
dt2[, mean(x1)]
dt2[,.(mean1 = mean(x1), sum2 = sum(x2))]

dt2[,lapply(.SD,mean)] # 모든 컬럼에 대하여

dt2[,lapply(.SD,mean),.SDcols=names(dt2)[1:10]]
dt2[,lapply(.SD,mean),.SDcols=paste0("x",1:10)] # can use text in .SDcols



# how to use 'by' & '.SD' -------------------------------------------------

dt3 <- data.table(
  A = runif(100),
  B = sample(letters[1:5], 100, replace=T),
  C = sample(LETTERS[1:5], 100, replace=T)
)
dt3

dt3[,.(mA =mean(A)), by = B]
dt3[,.(mA =mean(A)), by = .(B,C)]
dt3[,.(mA =mean(A)), by = c("B","C")]


dt3[,.(N = .N), by = B]
dt3[,.(N = nrow(.SD)), by = B][order(B)]

dt3[,.SD[1], by = B]


# sub query ---------------------------------------------------------------

# dt[][][]....


# text mining -------------------------------------------------------------

install.packages('KoNLP')
library(KoNLP)
useNIADic()


# apply -------------------------------------------------------------------

apply(iris[,1:4], 1, mean) # rows
apply(iris[,1:4], 2, mean) # columns

apply(iris[,1:4], 1, length) # rows
apply(iris[,1:4], 2, length) # columns

apply(iris[,1:4], 2, mean, na.rm=T) 


# lapply ------------------------------------------------------------------

lapply(iris, class) # list <- 각 변수에 대해 작업, 결과도 list
lapply(iris, class) %>% unlist # list to vector

lapply(1:10, function(x) x^2) %>% unlist # vector to list to vector
lapply(1:10, function(x, b) x^2+b^2, b=2)


# sapply ------------------------------------------------------------------

class(lapply(iris, class)) # list
class(sapply(iris, class)) # vector, auto unlist

sapply(1:10, function(x) x^2)
sapply(1:10, function(x) c(x^2, x^3)) # matrix


# tapply ------------------------------------------------------------------

tapply(iris$Sepal.Length, iris$Species, mean, na.rm=T)
class(tapply(iris$Sepal.Length, iris$Species, mean))



# sweep -------------------------------------------------------------------

irs <- sweep(iris[,1:4], 2, colMeans(iris[,1:4]),"-") # mean cleaning
colMeans(irs)

# ...and mapply, vapply


# file handling -----------------------------------------------------------

list.files(".","*.html") # (상위 디렉토리, 정규표현식)
list.files(".","^Lec")   # ^Lec : Lec로 시작되는 파일
list.files(".","ml$")    # ml$ : ml로 끝나는 파일
list.files(".","JES")    # JES 포함
list.files("..","*")     # 2개 상위, 

list.files(".","",include.dirs = F, recursive=F, full.names=T)

# http://www.nextree.co.kr/p4327/

file.info("LECTURE_5_-_Data_Munging_-_Part_I__tidyr_and_dplyr_.html")
file.path("data", "demo.splite")
file.exists("LECTURE_5_-_Data_Munging_-_Part_I__tidyr_and_dplyr_.html")

dir.create("ttt", recursive=F)
dir.create("ttt/aaa/bbb", recursive=F)


# string ------------------------------------------------------------------

substr("abcdef",2,4) # in excel = mid
grep("A", c("b","A","c","Apple")) # A가 들어가 있는 위치
grep("Sep", names(iris))

grep("Sep", names(iris), value=T)
grep("Length$", names(iris), value=T)

grep("Sep", names(iris), value=T, invert=T)

sub(" ",".","Hello There") # sub(a,b,c) >> a를 b로 바꿔라 c에서
gsub("e","E","Hello There")

grepl("Length$", names(iris))

strsplit("a-b-c","-") # tokenize
strsplit("a b c"," ") # tokenize

strsplit(c("a b c","b c r f")," ") # list

df <- data.frame(
  A = letters[1:5],
  B = LETTERS[1:5]
)
df

df$C <- paste0(df[,1],df[,2])
df

paste(df$A, collapse = ", ") # column 연결 / strsplit의 반대개념

# ?paste
# nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
# nth
# paste0(nth, collapse = ", ")
# paste("1st", "2nd", "3rd", collapse = ", ") # probably not what you wanted
# paste("1st", "2nd", "3rd", sep = ", ")
# paste(month.abb, nth, sep = ": ", collapse = "; ")

toupper("apple")
tolower(toupper("apple"))
str_to_title("I am a girl") # 제목 형태로 첫글자만 다 대문자로

# word(sentence, start point, end point)
sentences <- c("Jane saw a cat", "Jane sat down")
word(sentences, 1) # start와 end가 같은게 default
word(sentences, 2)
word(sentences, -1)
word(sentences, 2, -1)

word(sentences[1], 2)
word(sentences[1], 2, 4) # substr를 단어 단위로 한다
word(sentences[1], 1:3, -1)
word(sentences[1], 1, 1:4) 


# random number -----------------------------------------------------------

runif(5)
runif(5,1,2) # 범위 주어지는 거 가능

sample(100:200, 10, replace=T) # excel == randbetween

set.seed(5)
runif(5)

set.seed(100)
idx <- sample(1:150, 100)
train <- iris[idx,]
test  <- iris[-idx,]

nnorm()



# set operator ------------------------------------------------------------

"d" %in% c("A","B","C","D") # 포함여부
union() # 합집합
intersect() # 교집합
setdiff() # 차집합
setequal() # 두 집합이 정확히 일치해야 True

AllVar <- c(paste0("C",1:10), "yvar")
AllVar
notVar <- c("C1", "C10")
analX <- setdiff(setdiff(AllVar, notVar), "yvar")
analX


# data handling etc -------------------------------------------------------

df <- data.frame(
  
  score = sample(0:100, 1000, replace=T)
)
head(df)

cut(df$score, breaks=5)
df$grade <- cut(df$score, breaks=seq(0,100,by=20), labels = c("가","양","미","우","수"), include.lowest = T )
df

# Sys.setlocale("LC_ALL","korean")

head(df)
table(df$grade)
prop.table(table(df$grade))

install.packages('purrr')
library(purrr)
rerun(5,rnorm(5))
lapply(1:5, function(x) rnorm(5))

reduce(1:4, `+`) # 따옴표 개충격 , 누적계산
reduce(c("a","b","c"), paste0)

reduce_right(c("a","b","c"), paste0) # 거꾸로 계산
reduce(rev(c("a","b","c")), paste0)

cumprod(1:3) # [1] 1 2 6
reduce(1:3,`*`) # [1] 6
