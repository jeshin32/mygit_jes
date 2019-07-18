library(ggmap)
library(ggplot2)


# 1. Simple Example ----------------------------------------------------------

source = 'dsk'    # or 'google'

# from google
geocode('Seoul', source = source)  
geocode(enc2utf8('서울'), source = source)


# 2. Multiple Example ----------------------------------------------------------

# 위치이름을 df로 설정 : factor로 설정되면 안된다. 
city_df = data.frame(city = c('서울', '부산', '대전'), 
                     stringsAsFactors = F)
city_df

# 형변환 
city_df$city = enc2utf8(city_df$city)
city_df

# 위도 경도 정보 가져오기 
city_lonlat = mutate_geocode(city_df, city, source = source)
city_lonlat  


# 3. 2호선 정보 모두 가져오기 ---------------------------------------------------------- 

# 여러 지역 설정
# are_list = c('시청역', '을지로입구역', '을지로3가역', '을지로4가역', '동대문역사문화공원역', '신당역', '상왕십리역', '왕십리역', '한양대역', '뚝섬역', '성수역', '건대입구역', '구의역', '강변역', '잠실나루역', '잠실역', '신천역', '종합운동장역', '삼성역', '선릉역', '역삼역', '강남역', '교대역', '서초역', '방배역', '사당역', '낙성대역', '서울대입구역', '봉천역', '신림역', '신대방역', '구로디지털단지역', '대림역', '신도림역','문래역', '영등포구청역', '당산역', '합정역', '홍대입구역', '신촌역', '이대역', '아현역', '충정로역')
area_list = c("서울", "오산", "부산", "대구", "강원", "포항시", "평양", "단천", "천안")

# df로 변환 
area_df = data.frame(area_list, stringsAsFactors = FALSE)
area_df$area_list = enc2utf8(area_df$area_list)
head(area_df)

# 위도 경도 정보 가져오기 
area_latlon = mutate_geocode(area_df, area_list, source = source)
area_latlon


# 지도에 표시하기 
# map <- qmap('Seoul', zoom = 9,  maptype = 'hybrid')
# map +
#   geom_point(data = area_latlon, aes(lon, lat), size = 3, colour='#018b4d')

library(leaflet)
leaflet(area_latlon) %>% 
  setView(lng = 126.9778, lat = 37.56826, zoom = 6) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, color = "blue", weight = 1,
             radius = (1:9)^2/3)

