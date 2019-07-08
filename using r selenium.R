install.packages("RSelenium")
library(RSelenium)
library(rvest)

# 1) R에서 Selenium을 사용하려면 JAVA가 설치 되어 있어야 한다.
# 2) http://selenium-release.storage.googleapis.com/index.html     / Selenium standalone server
# https://selenium-release.storage.googleapis.com/index.html?path=&sort=desc
# https://github.com/mozilla/geckodriver/releases/tag/v0.17.0   /  gecko driver
# https://sites.google.com/a/chromium.org/chromedriver/          /  chrome driver
# 위에 3개의 품목을 다운받아 내 컴퓨터 C드라이버에 같은 폴더로 저장한다.
# 
# 
# 3) CMD에 입력  
# cd C:\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.8.1.jar -port 4445 (버전에 맞게 수정해준다)


remDr<- remoteDriver(port=4445L, browserName="chrome")
remDr$open()
#remDr$navigate("http://www.naver.com")

url <- 'https://page.onstove.com/talesrunner/kr/search/list/TITLE/view/3422260?listType=2&word=%EC%A3%BC%EC%82%AC%EC%9C%84&searchBoardKey=all&direction=latest'

remDr$navigate(url)
html <- remDr$getPageSource()[[1]] 
html <- read_html(html) 
View(html)
