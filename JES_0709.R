library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)


# review ------------------------------------------------------------------

slice() # using row name
select(iris, matches("Leng")) %>% head

iris %>% select_if(~!is.numeric(.)) # ~ : function, . : 인자
iris %>% select_if(function(x) !is.numeric(x))

iris %>% select(4:5) %>% distinct
select_if(iris, is.factor)
select_if(iris, is.numeric)
str(iris)

iris %>% filter_if(is.numeric, all_vars(.<4.5)) # 컬럼을 선택해서 조건검색, 도출되는 컬럼이 줄어들진 않음
iris %>% filter_at(vars(1:2), all_vars(.>4)) # 특정 컬럼에 대해서만 조건검색 하겠다

iris %>% group_by(Species) %>% slice(1:3)
iris %>% group_by(Species) %>% sample_n(3)

iris %>% select_if(function(x) n_distinct(x) >= 30)
iris %>% select_if(~(n_distinct(.) >=30))

iris %>% select_if(~(is.numeric(.) & mean(.)>3)) %>% names() # colnames()

# web scraping
library(rvest)

keyword = "자동차"
encoding = "UTF-8"

url = "https://www.google.com/search?q=--KEYWORD--&source=lnms&tbm=isch&sa=X&ved=0ahUKEwjT25z3z6bjAhVkI6YKHY0ZCl0Q_AUIECgB&biw=1920&bih=963"
url <- sub("--KEYWORD--", keyword, url)
url

page <- read_html(url, encoding = encoding)
page

links <- html_nodes(page, "a img") %>% html_attr("src")
links

# k=0
# for (li in links) {
#   ???
# }

# example
df <- data.frame(
  A=1:3,
  B=c("1234","3456 7890", "4321 3225 7894")
)
df


df_b <- paste(df[1,2], df[2,2], df[3,2])
df_b_split <- strsplit(df_b, " ")

new_df <- data.frame(
  A = 1:6,
  B = strsplit(paste(df[1,2], df[2,2], df[3,2]), " ")
)
new_df

install.packages("splitstackshape")
library(splitstackshape)
cSplit(df,"B"," ",direction = "long")

# data load

df <- dbGetQuery("SELECT * FROM...")

total <- reduce(lapply(), merge, by=key, all.x=T)

# 이변량 분석 : y와의 관계

