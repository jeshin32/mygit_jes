library(rvest)

# 1. 해당 url을 새로운 변수에 저장한다 --------------
url = "https://news.naver.com/main/clusterArticles.nhn?id=c_201810211210_00000007&mode=LSD&mid=shm&sid1=101"

# 2. url에서 html파일을 읽어오고 저장한다 -----------
html = read_html(url)

# 3. 읽어 오기 
html %>% html_nodes("li dt a") %>% html_text() %>% 
  grep("\n                            \n                            ", . , invert = T, value = T) %>% 
  gsub("\"", "", ., fixed = TRUE)



# 4. 내용 가져오기 
links <- html %>% html_nodes("li dt a") %>% html_attr("href") %>% unique()
links


titles <- list()
contents <- list()

for(i in 1:length(links)) {
  
  url = links[i]
  html = read_html(url)
   
  
  titles[i] <- html %>% html_nodes("#articleTitle") %>% html_text()
  contents[i] <- html %>% html_nodes("#articleBodyContents") %>% html_text() %>% 
    gsub("\n", "", .) %>% gsub("\t", "", .) %>% gsub("//", "", .) %>% 
    gsub("[\\{\\}\\(\\)]", "", .) %>% 
    gsub(" flash 오류를 우회하기 위한 함수 추가function _flash_removeCallback()", "", .) 
  
}

out <- data.frame(title = unlist(titles),
                  contents = unlist(contents))
out

write.csv(out, "naver_news.csv")


