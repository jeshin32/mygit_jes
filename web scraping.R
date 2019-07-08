
# web scraping : rvest ----------------------------------------------------

install.packages("rvest")
library(rvest)

# html structure >> tree

url_tvcast = "http://tvcast.naver.com/jtbc.youth" 
html_tvcast = read_html(url_tvcast, encoding = "UTF-8")
html_tvcast %>% html_nodes(".title a") 
tvcast_df = html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()
tvcast_df 

url_wiki = "http://en.wikipedia.org/wiki/Student%27s_t-distribution"
html_wiki = read_html(url_wiki, encoding = "UTF-8") 
html_wiki %>% html_nodes(".wikitable") %>% html_table(header = FALSE) %>% data.frame()

# html_text() # 텍스트를 추출한다
# html_name() # attribute의 이름을 가져온다
# html_children() # 해당 요소의 하위 요소를 읽어온다.
# html_tag() # tag이름 추출
# 
# html_attrs() # attribute을 추출한다

url_news = "https://news.naver.com/"
html_news = read_html(url_news)
#news_df = html_news %>% html_nodes(".mtype_list_wide a") %>% html_text() %>% data.frame()
news_df = html_news %>% html_nodes(".mlist2 a") %>% html_text() %>% data.frame()
news_df$. <- trimws(news_df$.)
news_df


urls = html_news %>% html_nodes(".mlist2.no_bg a") %>% html_attr("href") %>% data.frame()
#str(urls)

for(i in 1:10) {
  i = 1
  
  url <- as.character(urls[i,1])
  #url
  html <- read_html(url)
  #html
  text <- html %>% html_nodes("#articleBodyContents") %>% html_text() 
  print(text)
  
}

samples = c('Sample 1', 'Sample 1 ')
trimws(samples)

# node : 이름
# class = "_" . __
# id = "_" # __