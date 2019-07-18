
library(rvest)


###############################################################################################
# I. 예제로 네이버 tv캐스트 <청춘시대>에 올라온 동영상 목록들을 긁어와 보도록 한다 --------------
###############################################################################################

# 1. 해당 url을 새로운 변수에 저장한다 --------------
url_tvcast = "http://tvcast.naver.com/jtbc.youth" 

# 2. url에서 html파일을 읽어오고 저장한다 --------------
html_tvcast = read_html(url_tvcast, encoding = "UTF-8") 


# 3. 크롬 개발자도구를 열어 구조 살펴 보기  --------------

# #이런 형식 ㅇㅇ
# <dt class = "tilte">
#   <a>
#   <tooltip>여기에 제목이 있넹</tooltip>
#   </a>
#   </dt>


# 4. html_node() / html_nodes() 함수는 css, xpath로 원하는 부분을 추출할 수 있다.   --------------
# tag, class, id 모두로 찾을 수 있다는 말이다. 

html_tvcast %>% html_nodes(".title a")  


# 5. html_text()로 그 안에 있는 text만 추출, data.frame으로 저장한다   --------------

tvcast_df = html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame() 
tvcast_df

###############################################################################################
# II. table 추출  --------------
###############################################################################################

# 1. 해당 url을 새로운 변수에 저장한다 --------------
url_wiki = "http://en.wikipedia.org/wiki/Student%27s_t-distribution"

# 2. url에서 html파일을 읽어오고 저장한다 --------------
html_wiki = read_html(url_wiki, encoding = "UTF-8") 


# 3. #table의 class명인 "wikitable을 html_table()이용해서 가져온다"   --------------

html_wiki %>% html_nodes(".wikitable") %>% html_table()





