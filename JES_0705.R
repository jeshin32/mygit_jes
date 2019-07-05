

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

