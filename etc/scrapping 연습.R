

library(rvest)

# 청춘시대 --------------------------------------------------------------------------

url_tvcast = "http://tvcast.naver.com/jtbc.youth" 



# html의 code 자체를 읽어 가지고 오는    read_html
html_tvcast = read_html(url_tvcast, encoding = "UTF-8") 
html_tvcast

# 특정 tag 부분을 읽어오는     html_nodes 
html_tvcast %>% html_nodes(".title a") 

# 텍스트 부분만 읽어오는   html_text
tvcast_df = html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()
tvcast_df 



# wiki --------------------------------------------------------------------------


url_wiki = "http://en.wikipedia.org/wiki/Student%27s_t-distribution"

html_wiki = read_html(url_wiki, encoding = "UTF-8") 
html_wiki

df = html_wiki %>% html_nodes(".wikitable") %>% html_table() 
df
class(df)
class(df[[1]])

df <- df[[1]]
df
head(df)
df <- df[-1,]
head(df)

# news --------------------------------------------------------------------------





library(rvest)

# 1. 해당 url을 새로운 변수에 저장한다 --------------
url = "https://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=100&sid2=264"

# 2. url에서 html파일을 읽어오고 저장한다 -----------
html = read_html(url)
html

# 3. 읽어 오기 
html %>% html_nodes("li dt a") %>% html_text()

# 이상문자 제거 
library(stringr)
library(dplyr)
library(magrittr)


df <- 
html %>% html_nodes("li dt a") %>% html_text() %>% 
  str_trim()  %>%       # 앞뒤 공백문자 제거
  data.frame 

names(df) <- "title"  
  
df %<>% filter(title != "")
  


