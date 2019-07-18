library(rvest)

# 1. 해당 url을 새로운 변수에 저장한다 --------------
url = "https://news.naver.com/main/list.nhn?mode=LS2D&mid=shm&sid1=100&sid2=264"

# 2. url에서 html파일을 읽어오고 저장한다 -----------
html = read_html(url)

# 3. 읽어 오기 
html %>% html_nodes("li dt a") %>% html_text() %>% 
  str_trim()

# 4. 내용 가져오기 
links <- html %>% html_nodes("li dt a") %>% html_attr("href") %>% unique()
links


titles <- list()
contents <- list()

library(magick)

for(i in 1:length(links)) {
  
  url = links[i]
  html = read_html(url)
   
  
  titles[i] <- html %>% html_nodes("#articleTitle") %>% html_text()
  contents[i] <- html %>% html_nodes("#articleBodyContents") %>% html_text() %>% 
    gsub("\n", "", .) %>% gsub("\t", "", .) %>% gsub("//", "", .) %>% 
    gsub("[\\{\\}\\(\\)]", "", .) %>% 
    gsub(" flash 오류를 우회하기 위한 함수 추가function _flash_removeCallback()", "", .) 
  
  
  
  # image download
  imageURL <- url %>% read_html %>% html_nodes(".end_photo_org img") %>% html_attr("src")
  print(imageURL)

  # download.file(url = imageURL, destfile = paste0("Image/", i, ".jpg"))
  img <- image_read(imageURL)
  image_write(img, paste0("Image/", i, ".jpg"))
}

out <- data.frame(title = unlist(titles),
                  contents = unlist(contents))
out

write.csv(out, "naver_news.csv")



