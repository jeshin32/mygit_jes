rm(list = ls())
gc(reset = T)   # K.clear_session

library(data.table)
library(tidyverse)
library(h2o)   # 변수명 한글을 지원 안 한다
library(dplyr)


# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     01.데이터 불러오기     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #

customer <- fread('./data/customer.csv', stringsAsFactors = T) # encoding기법을 지원을 안 함, 주의해서 사용해야 한다
contract <- fread('./data/contract.csv', stringsAsFactors = T)
guarantee <- fread('./data/guarantee.csv', stringsAsFactors = T)

head(customer)
dim(customer)

# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      02.데이터 전처리      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #

## --2.1. contract guarantee 조인--

contract
guarantee

contract[duplicated(INS_NO),.N] ## 중복 확인 / .N = counting number

setkey(contract, 'CUST_ID', 'INS_NO')
setkey(guarantee, 'CUST_ID', 'INS_NO')

# 질문1 ----------------- 
cont_gua_dat <- guarantee[contract] # contract를 기준으로 guarantee를 붙인다
head(cont_gua_dat)
# unique(substr(contract$INS_NO,16,20))
# length(unique(guarantee$INS_GRP_NM))

# FROM CONTRACT
# LEFT JOIN GUARANTEE
# condat <- right_join(contract, guarantee, by = c('CUST_ID', 'INS_NO'))
# head(condat)


## --2.2. cont_gua_dat 데이터 전처리--

cont_gua_dat <- cont_gua_dat[!is.na(INS_GRP_NM)] ## 종목군구분명 없는 건수 제외하기

cont_gua_dat[, BAS_YMD := as.Date(paste0(BAS_YM,'31'),'%Y%m%d')] ## 특약 담보개시일자부터 기준년월까지의 일자 계산
str(cont_gua_dat$TRET_GURT_OPEN_YMD)
cont_gua_dat[, TRET_GURT_OPEN_YMD := as.Date.character(TRET_GURT_OPEN_YMD,'%Y%m%d')]

cont_gua_dat[, ELSP_DAY := as.numeric(BAS_YMD - TRET_GURT_OPEN_YMD)]
# cont_gua_dat[, ELSP_DAY := BAS_YMD - TRET_GURT_OPEN_YMD]

cont_gua_dat[,unique(TRET_GURT_NM)]
unique(cont_gua_dat$TRET_GURT_NM) ## 같은 결과

# 질문2 ----------------- 
length(unique(cont_gua_dat$TRET_GURT_NM))  # 현 데이터 상황의 정보
length(levels(cont_gua_dat$TRET_GURT_NM))  # 원천 데이터의 정보, 데이터를 지운다고 없어지진 않는다

cont_gua_dat <- cont_gua_dat[,-c('TRET_GURT_NM', 'TRET_GURT_OPEN_YMD', 'TRET_GURT_END_YMD', 'BAS_YM', 'BAS_YMD')]


cont_gua_summary <- cont_gua_dat[,.(WON_JOIN_AMT = sum(WON_JOIN_AMT), 
                                    WON_BASE_PRE_AMT = sum(WON_BASE_PRE_AMT),
                                    PAY_INS_AMT = sum(PAY_INS_AMT),
                                    INS_GRP_CNT = .N,
                                    ELSP_DAY = mean(ELSP_DAY)), c('CUST_ID','INS_GRP_NM')] ## CUST_ID / INS_NO / INS_GRP_NM ?
cont_gua_summary

levels(cont_gua_summary$INS_GRP_NM) <- c('01','02','03','04','05','06','07')

vars <- names(cont_gua_summary)[-c(1,2)]
# 질문3 ----------------- 
cont_gua_summary <- cont_gua_summary %>% 
                    dcast(CUST_ID~INS_GRP_NM, value.var = vars, fill = 0)


## --2.3. customer 데이터 전처리 --

str(customer)

# JOB_NM
customer[, JOB_NM := str_replace_all(JOB_NM, ' ', '')]
customer[JOB_NM == '',.(JOB_NM)]
customer[, JOB_NM := ifelse(JOB_NM == '',NA, JOB_NM)] ## ''를 NA 값으로 바꾸기
customer[is.na(JOB_NM),.(JOB_NM)]

# MAKING Y
customer[, EXIT_CD := ifelse(!is.na(FINL_EXIT_YMD), 'Y','N')] ## Target 만들기

# AGE
customer[, c(quantile(AGE), 
             MEAN = mean(AGE),
             SD = sd(AGE))]

cut(customer$AGE, breaks = c(0, 20, 30, 40, 65, 90),
                  labels = c('어린이', '20대', '30대', '중장년층', '노년층'))
customer[ ,AGE_CD := cut(customer$AGE, breaks = c(0, 20, 30, 40, 65, 90),
                        labels = c('어린이', '20대', '30대', '중장년층', '노년층'))]


## --2.3. customer 데이터 조인 --

customer
cont_gua_summary

setkey(customer, 'CUST_ID')
setkey(cont_gua_summary, 'CUST_ID')

final_dat <- cont_gua_summary[customer]


## --2.4. final_dat 형변환 --
# 고객 ID가 들어가면 사람에 따라 TREE를 형성, OVERFITTING 될 수 있다
# 따라서 고유값/키값은 제외하자

# 변수명 global setting
grep('YM', names(final_dat), value=T) # pattern

char_var <- grep(c('_ID|_YM|_YMD|NO'), names(final_dat), value = T)
fact_var <- grep(c('_CD|_YN|_NM'), names(final_dat), value = T)
num_var <- setdiff(names(final_dat), c(char_var,fact_var))  # c(char_var,fact_var)와 다른 변수들을 찾기

length(num_var) + length(fact_var) ## 변수가 모두 들어갔는지 확인하기


# 형태변환
lapply(final_dat$AGE, function(x) x-1)
## lapply(final_dat$AGE, function(x) {sum(x)}) = lapply(final_dat$AGE, sum)

final_dat[, char_var := lapply(.SD, as.character), .SDcols = char_var]
## error : 안 치면 변수를 만든다
final_dat <- final_dat[,-c('char_var')]

# 질문4 ----------------- 
final_dat[, (char_var) := lapply(.SD, as.character), .SDcols = char_var]
## 괄호 넣는 걸 잊지 말 것, 변수들을 캐릭터화 >> char_var라는 벡터값을 넣어준 것이다 (변수명으로)
## mutate냐 replace냐

final_dat[, (fact_var) := lapply(.SD, as.factor), .SDcols = fact_var]
final_dat[, (num_var) := lapply(.SD, as.numeric), .SDcols = num_var]
final_dat[, (num_var) := lapply(.SD, function(x) ifelse(is.na(x), 0,x)), .SDcols = num_var]
## num_var에 NA 값을 0으로 변환하기


# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       03. 모형 생성        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ #

set.seed(12345)

idx <- sample(nrow(final_dat), nrow(final_dat)*0.7)

train_dat <- final_dat[idx]
valid_dat <- final_dat[-idx]

h2o.init()  # default가 nthreads = -1

train.hex <- as.h2o(train_dat)
valid.hex <- as.h2o(valid_dat)

model <- h2o.randomForest(x = c(fact_var[-13], num_var),
                          y = fact_var[13],
                          
                          training_frame = train.hex, 
                          validation_frame = valid.hex,
                          
                          ntrees = 200,
                          max_depth = 5,
                          mtries = -1)

h2o.gainsLift(model)
model

pred <- as.data.table(h2o.predict(model, valid.hex))
pred

h2o.shutdown(prompt = F)

fwrite()
