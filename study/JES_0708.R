
# review ------------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(purrr)
library(ggplot2)


dt[,mean(A_PRICE),
   by = .(M_GU_NAME,M_TYPE_NAME)] # ""를 사용하지 않음

dt[,mean(A_PRICE),
   by = .(M_GU_NAME,M_TYPE_NAME)] %>% 
  spread(M_TYPE_NAME, V1)


# select
dt[,A]
dt[,"A"]
dt[,1]

dt[,list(A,C)]
dt[,.(A,C)]
dt[,c("A","C")] # df grammar

# mutate
dt[,new2 := sample(1:3,5,replace=T)]
dt[,":="(n1=1,n2=2)]
dt[,c("nn1","nn2") := .(3,4)]  # ***


# mutate 응용
dt2 <= data.table(a=1:6)
dt2

dt2[, paste0("x",1:10) := lapply(1:10, function(x) runif(6))]
dt2
dt2[, paste0("x",1:10) := lapply(paste0("x",1:10), function(x) {dt2[,get(x)]*10})]
dt2

# summarise
dt2[, .(mx1 = mean(x1),
        mx2 = mean(x2))]
dt2[, lapply(.SD, mean)]
dt2[, lapply(.SD, mean), .SDcols = names(dt2)[1:10]]
dt2[, lapply(.SD, mean), .SDcols = paste0("x",1:10)]

dt2$cate <- sample(letters[1:3], 6, replace=T)
dt2[, lapply(.SD, mean), by=cate]

# group by
dt3[, .(N=.N), by=B]
dt3[, .(N=nrow(.SD)), by=B]

dt3[, .SD[.N], by=B] # pick the last row


# functions
seq(4) # 1,2,3,4 
mat1 <- matrix(rep(seq(4),4), ncol=4)
apply(mat1, 1, sum) # 1 = row , 2 = column

apply(mat1, 1, function(x,y) sum(x) + y, y=3) # 넘기는 값을 list로 만들어서 a,b를 한 번에 넘긴다

y <- unlist(lapply(1:5, function(i) print(i)))
class(y)
y

tapply(iris$Sepal.Length, iris$Species, mean) # like a table group by, 결과값은 array


list1 <- lapply(1:6, runif)
list1
list2 <- lapply(1:6, runif)
list2
lapply(1:6, function(i, x, y) x[[i]] + y[[i]],
       x = list1, y = list2)

?dir

x <- "abcdef"
substr(x, 2, 4) 
substr(x, 2, 4) <- "123456"  # 길이 맞춰 쓰고 버림
x

grep("^A", list.files('.'), value = T)
list.files('.', pattern="^A")
?grep

grep("A", c("b", "A", "c", "Aapple"), value = T, invert = T)
grepl("A", c("b", "A", "c", "Aapple")) # true&flase

sub("\\s", ".", "Hello There")
sub(" ", ".", "Hello There")
sub(" ", ".", "Hello There", fixed = T) # 일반문자열임을 정확하게 표현하기 위한 조건값

strsplit("abc","")
x <- c("asf.f", "qwerty.123", "apple.orange", "banana")
x
strsplit(x, ".", fixed = T)
unlist(strsplit(x, ".", fixed = T)) # 전부 타 vector로 연결해줌

strsplit("a.b.c", ".")
strsplit("a.b.c", "[.]")
strsplit("a.b.c", ".", fixed = T)

## a useful function: rev() for strings

x <- "abc"
strReverse <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "") # collapse : 벡터를 뭉개줌
}
strReverse(c("abc", "Statistics"))

?rev

"k" %in% letters # 있는지 없는지

(x <- c(sort(sample(1:20, 9)), NA)) <- () : PRINT after assign 

library(purrr)
y <- unlist(rerun(10000, sample(1:45,6)))
table(y)


set.seed(1)
samples <- rerun(3, sample(10,5))
samples
union(union(samples[[1]], samples[[2]]), samples[[3]])
samples %>%  reduce(union) # REDUCE = cumsum !



# review - etc functions --------------------------------------------------

library(randomForest)
randomForest(Species ~ Sepal.Length + Sepal.Width,
             data = iris, ntree=200)

form = "Species ~ Sepal.Length + Sepal.Width"
randomForest(formula(form), data = iris, ntree=200)
form = paste0("Species ~ ",
              paste0(names(iris)[1:3], collapse = " + "))
form

levels(iris$Species)

which.max(c(3,6,7,1)) # 자리값을 돌려줌
which(c(1,4,5,7) < 5 )

System.time() # 속도측정
Sys.time() # 현재시간

cumsum(c(1,3,5)) # 누적합0

table(iris$Species)
prop.table(table(iris$Species))

x <- 1:3
y <- 5:7
rbind(x,y) # row bind
cbind(x,y) # column bind

gtools:smartbind()

library(plyr)
a <- sample(letters[1:3], 10, replace=T)
a
mapvalues(a, c("a","b","c"), c("APPLE","BANANA","CARET")) # recode() in dplyr

a <- c(1,2,4,NA,5,6, NA)
a
coalesce(a, 100) # NA 바꿀 때
replace_na(a, 100) # in tidyr

glimpse(iris) # == str(iris)

sample_n(iris, 3) # 개수로 뽑기
iris %>% group_by(Species) %>% 
  sample_n(3)
iris %>% data.table() %>% 
  .[, sample_n(.SD, 3), by=Species]

sample_frac(iris, 0.05) # 비율대로 뽑기

colSums(iris[,1:4])
rowSums(iris[1:4,1:4])

?cov

a <- runif(100)
quantile(a, probs = 0.1)
quantile(a, probs = 0.5)   # median(a)
quantile(a, probs = c(0.1,0.5,0.9))



# advanced dplyr ----------------------------------------------------------


iris %>% randomForest(Species ~.,
                      data = .,
                      ntree=200) # 데이터가 중간에 나오면 . <- 이걸로 해서 %>% 쓰면 됨

iris %>% select(Sepal.Length, Species) %>% 
  .[lapply(.SD, mean), .SDcols = Species] ########

slice(iris, 105)
slice(iris, c(1,3,5))

select(iris, starts_with("Se")) %>% head()
select(iris, ends_with("Length")) %>% head()
select(iris, contains("Len")) %>% head()

iris %>% rename(Spec = Species) %>% head() 

iris %>%  select(4:5) %>% distinct

myfun <- function(x) {
  mean(x)>4 & is.numeric(x)
}
select_if(iris, myfun)
colMeans(iris[,1:4])

iris %>% select_if(is.numeric) %>% filter_all(all_vars(.<4.5)) # and condition
iris %>% select_if(is.numeric) %>% filter_all(any_vars(.>7)) # or condition

# select는 아니기 때문에 모든 컬럼이 결과값으로 나오기는 함
# 조건에 있어서의 제약만 주었을 뿐 
iris %>% filter_if(is.numeric, all_vars(.<4.5)) # filter_if(select condition, condition)
iris %>% filter_at(vars(1:2), all_vars(.>4)) # filter_at(select condition, condition)

iris %>% mutate_all(toupper)
iris %>% mutate_if(is.numeric, function(x) x*10) %>% head()
iris %>% mutate_at(vars(contains("Length")), ~(.*100)) %>% head()
# ~ : 함수, . : 선택된 변수

iris %>% transmute(SL=Sepal.Length*10) %>% head() # 최종 생성되는 변수만 남김

iris %>% summarise_all(mean, na.rm=T)
iris %>% summarise_if(is.numeric,mean)
iris %>% group_by(Species) %>% summarise(n()) # all counts
iris %>% group_by(Species) %>% summarise(n_distinct(Petal.Width)) # unique counts
iris %>% group_by(Species) %>% add_count() %>% head(20)


iris %>% group_by(Species) %>% distinct(Petal.Width) %>% head(20)
View(iris %>% group_by(Species) %>% distinct(Petal.Width))

iris %>% group_by(Species) %>% first() # first vector
iris %>% group_by(Species) %>% last() # last vectora


# using database ----------------------------------------------------------

install.packages("DBI")
install.packages("RSQLite")
install.packages("dbplyr")

library(DBI)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/Uxxxxxxx/Downloads/HD - Mid 201906/RAdvancedFunctions/Data/demo.sqlite")
dbListTables(con)
dbTable <- tbl(con, "albums") # ****
dbTable %>% head()

dbTable %>% filter(ArtistId <=10) %>% 
  group_by(ArtistId) %>% summarise(N=n()) # %>% View()


# library magrittr --------------------------------------------------------

# exposition operator
install.packages("magrittr")
library(magrittr)

mean(iris$Sepal.Length)
with(iris, mean(Sepal.Length))
iris %$% mean(Sepal.Length)

cor(iris$Petal.Length, iris$Petal.Width)
iris %$% cor(Petal.Length, Petal.Width)

#compound assignment operator
x <- 1:10
x <- round(log(x),2)
x

x <- 1:10
x %>%  log %>% round(2)
x
x %<>% log %>% round(2)   # x <- x %>% log %>% round(2)

# tee operator
rnorm(5) %T>%
  print %>% # 얘는 건너 뛰는거야 , 출력만
  mean

iris %>% select_if(is.numeric) %T>%
  head %>%
  colMeans

library(ggplot2)
iris %T>% ggplot(aes(Sepal.Length, Sepal.Width)) + geom_point() %>% head() # 얜 안되고
iris %T>% head() %>% ggplot(aes(Sepal.Length, Sepal.Width)) + geom_point() # 앤 되는데 head() 안 되고



