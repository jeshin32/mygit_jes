
read.csv("Data/wide_data.csv")

ls()

rm(a)
ls()

rm(list=ls())
ls()

iris
head(iris)


# chap1 -------------------------------------------------------------------

library(networkD3)


mylinks <- read.csv("Data/MyLinks.csv")
mynodes <- read.csv("Data/MyNodes.csv")

forceNetwork(Links = mylinks, Nodes = mynodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)
getwd()

head(mylinks)
head(mynodes)


# chap2 -------------------------------------------------------------------

install.packages("leaflet")
library(leaflet)

leaflet() %>% addTiles() %>%
  addMarkers(lng= 126.98, lat= 37.54)

leaflet() %>% setView(lng = 126.98, lat = 37.54, zoom = 12) %>% 
  addTiles() %>% 
  addProviderTiles(providers$Stamen.Toner)


# chap3 -------------------------------------------------------------------

#생성
x <- c(1,2,3,4,5) # make vector
x

#추출
x[2]
x[5] # 위치가 몇 번째인지
x[2,5] # error
x[c(2,5)] # 위치 정보를 벡터로 그룹핑하여 넣어 준다

1:3
5:10
5;10
x[3:5]
x[1:4]

# 수정
x[3] <- 100
x[3]
x
x[3:5] <- 200
x

x <- 1:5
x[3:5] <- x[3:5]*10
x


# function ----------------------------------------------------------------

length(x)

#연속 패턴의 데이터를 만들어 주는 함수
seq(1,5, by=2) 
seq(1,100, by=2)
seq(0,1, by=0.1)
seq(10,1, by=-0.5)

seq(1, length=10, by=4)

x <- 1:3
x
rep(x,3)
rep(x,10)
rep(x,each = 5)


# characther --------------------------------------------------------------

v <- c("orange","banana","apple")
v
v[2]

#하나의 문자열로 합쳐줌
paste("X", "Y", "Z") #default=balnk
paste("X", "Y", "Z", sep="_")
paste("X", "Y", "Z", sep="")
paste0("X","Y","Z")


# caution -----------------------------------------------------------------

x <- c(1,1,1,1,1,1)
y <- 1:3
x
y
z = x + y
#111111
#123123 <- repeat
z

x <- 1:5
x
1/x
x+2
#elementwise로 계산해준다

# 조건추출 --------------------------------------------------------------------

X <- 1:10
X
X[ X>7 ]
X>7
X[ X%%2==1 ]
X[ (X>=5) & (X%%2==1) ]


# 특이상수 -------------------------------------------------------------------

x <- c(1,2,3,4,5,NA,7,8,9,NA)
x
x[ x>7 ] #na는 판단하지 않고 걍 내뱉는다
x[ x>7 & !is.na(x) ]

sum(1:10)
sum(x, na.rm=T)
mean(x, na.rm=T)
1/0 
-1/0
Inf - Inf # NaN : Not a Number

#x1,......x20
paste0("x", 1:20) #앞뒤가 길이 다르니까 앞에를 20번 반복해주게 되는 것
paste0("x_", 1:20)
paste("x", 1:20, sep="_")

paste0(c("x","y",'z'),1:10)

idx = rep(c("x","y","z"), each=10)
idx
paste0(idx,1:10)


# etc ---------------------------------------------------------------------


#7의 배수의 개수 구하기
x <- 1:1000
x
x %% 7 == 0
sum(x %% 7 == 0)
mean(x %% 7 == 0) # 7의 배수 비율

#반복하여 벡터 뽑기
x <- 1:3
x[c(1,1,3)]
x <- c("x","y")
x[c(1,2,2,1)]

xx <- c("x","y")
xx[ rep(c(1, 2, 2, 1), times = 2) ]
# 안 부터 먼저 실행시킨다

#제외하고 뽑기
x <- 1:10
x
x[5]
x[-5]
x[-c(1,5)]

v <- 1:19
v[ v %% 2 == 0 ]
v[seq(2,19,by=2)]
v[(1:9)*2]
v

idx = seq(1, 19, by=3)
idx
v[idx]

paste0(rep(c("x","y","z"),each=10), 1:10)


# matrix ------------------------------------------------------------------

#생성
z <- 1:30
dim(z) <- c(6,5)
z
dim(z) <- c(2,5,3)
z

#추출
z[3,4]
z
z[6,5]
z[2,] #all
z[c(2,4),]

z[,3] #하나만 뽑으면 벡터 형태로
z[,3,drop=F] #하나만 뽑아도 dim 유지*

z[,c(3,5)]

#수정
z[,3] <- 0 
z
z[,5] <- z[,5]*10
z


# matrix function ---------------------------------------------------------

mat <- matrix(1:30, nrow=6) #행 또는 열 하나만 개수 지정하면 됨
mat <- matrix(1:30, ncol=5)
mat

mat <- matrix(1:30, ncol=5, byrow = T)
mat

idx <- matrix(c(1,3,2,2,3,1), nrow=3, byrow=T) #패턴이 없을 경우
mat[idx]

mat[c(1,3),c(1,3)] #행조건, 열조건 패턴이 있을 경우

dim(mat)
nrow(mat)
ncol(mat)
length(mat) #number of data

x <- 1:5
length(x)
dim(x) #vector has no dim

dim(mat)[1]
dim(mat)[2] #dim(mat)까지가 vertor라서 문법을 뒤에 이어서 쓴 것

names(iris) #변수 이름만 쭉 뽑고 싶을 때
names(iris)[1:3]

mat <- matrix(1:30, nrow=5)
mat


# list --------------------------------------------------------------------

#생성
lst <- list(
  name = "Kevin",
  age= 30,
  height = 180,
  cert = c("A","B","c"),
  mat = matrix(1:6, nrow=3)
)
lst #변수명*값으로 출력됨
lst2 <- list(
  name = "Jane",
  age= 20,
  height = 160,
  cert = c("D","E"),
  mat = matrix(2:7, nrow=3)
)
lst2

lst_all <- rbind(lst,lst2)
lst_all[[7]]
lst_all[,1]


#추출
lst$cert
lst$mat[,2, drop=F]
lst$age
lst[2]

md <- lm(Sepal.Length ~ Petal.Length, data = iris)
md$coefficients
md$model
md$residuals

lst[1] #[]: 리스트의 부분집합을 추출 = 결과도 리스트
lst[3:4]

class(lst[1])
class(lst[3:4])

length(lst[1])
length(lst[3:4])
length(lst)

lst[4][2] #error
lst$cert[2]
lst[[4]] #[[]]: 값을 추출
lst[[4]][2]

class(lst[[4]])
length(lst[[4]])

lst[[5]]
lst[[5]][,2]

#변수추가
lst$new <- 1:10
lst

#변수삭제
lst$new <- NULL
lst


# data frame --------------------------------------------------------------

df <- data.frame(
  A = 1:5,
  B = 5:1,
  C = letters[1:5]
)
df

df$A
df[1] #[]: 부분집합 == 결과가 data.frame
class(df[1])
length(df[1])

df
df[[1]][4] # df[1][4] = error, [[]]: 값을 추출(vector)

#matrix 문법도 그대로 사용 가능
df[4,1]
df[4,3]


# data frame function -----------------------------------------------------

dim(df)
nrow(df)
ncol(df)
length(df)

head(iris)
dim(iris)

summary(iris)
View(iris)
str(iris)


# data handling -----------------------------------------------------------

head(iris)
iris[iris$Sepal.Length>7&iris$Sepal.Width>3 , c(1,3,5)] #행조건,열조건 순서!!

summary(iris)
iris[iris$Species=="versicolor",]

iris$id <- 1:150
head(iris)

iris$id <- NULL
head(iris)

iris$Petal.Length <- iris$Petal.Length*10
head(iris)
