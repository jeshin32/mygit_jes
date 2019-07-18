library(rvest)

keyword ="자동차"

url = "https://www.google.co.kr/search?q=--KEYWORD--&hl=ko&source=lnms&tbm=isch&sa=X&ved=0ahUKEwjXipyZ8JzeAhWJabwKHXBKCi8Q_AUIDygC&biw=1229&bih=540"
url <- sub("--KEYWORD--", keyword, url)
url

page <- read_html(url, encoding ="CP949")
page

links <- html_nodes(page, "a img") %>% html_attr("src")
links

k=0
for (lk in links) {
  k=k+1
  download.file(lk, destfile =paste0("Image/", k, ".png"), mode = 'wb')
}


