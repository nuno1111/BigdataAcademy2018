
land_price <- read.csv("./파일럿프로젝트/DATA/제주특별자치도_서귀포시_개별공시지가정보_20170101.csv")

############################# 제주도 날씨 예측 #############################

poi <- read.csv("./파일럿프로젝트/DATA/제주특별자치도_2015년_제주도_장소_POI데이터_파일_2번_최종_.csv")

library(sqldf)

colnames(poi) <- c("ID","X","Y","TYPE","NAME","TEL","TEL2","FAX","ADDR","NEW_ADDR")

(sqldf("SELECT DISTINCT TYPE FROM poi"))
(sqldf("SELECT NAME FROM poi WHERE TYPE = '레져/스포츠'"))

############################# 위경도 좌표계 변환 #############################
library(sp)
library(rgdal)

# 좌표계 변환 함수
convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("utmk_long", "utmk_lat")
  
  return(changed)
}

coord <- data.frame(long=poi$X, lat=poi$Y) # 위경도 데이터 추출

# 일반 위경도 좌표계
from.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

# 300 그리드에서 사용된 좌표계 utmk
to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs" 

# 좌표계 변환 수행 후 컬럼 추가
coord <- cbind(coord, convertCoordSystem(coord$long, coord$lat, from.crs, to.crs))
poi <- cbind(poi, coord[,c("utmk_long","utmk_lat")])

############################# 300 GRID 격자에서 ID별 위경도 추출 #############################

library(rgdal)
lnd <- readOGR(dsn = "./파일럿프로젝트/GIS", layer = "PCELL_ID_300") # GIS SHP 파일 로딩

lnd_data <- lnd@data

# ID_300 별 X,Y좌표만 추출하여 매핑
t1 <- lapply(lnd@polygons, function(x){x@Polygons[[1]]@coords[1,]})
t2 <- unlist(t1) # vector
t3 <- matrix(t2, ncol = 2, byrow = T) # matrix
result <- as.data.frame(t3) # data.frame
colnames(result) <- c("utmk_long","utmk_lat")

lnd_data <- cbind(lnd_data,result)

############################# STORE 데이터에 ID_300 매핑 #############################

library(sqldf)
library(data.table)

lnd_table <- data.table(lnd_data)
setkey(lnd_table,utmk_long,utmk_lat)

poi_id <- sqldf
("
     SELECT 
     A.*,B.ID_300 
     FROM poi A 
     LEFT OUTER JOIN lnd_table B
     ON A.utmk_long >= B.utmk_long 
     AND A.utmk_long < B.utmk_long+300 
     AND A.utmk_lat >= B.utmk_lat
     AND A.utmk_lat < B.utmk_lat+300  
")

str(poi_id)
plot(poi_id$X,poi_id$Y,col = poi_id$TYPE,cex = .3)

############################# STORE 데이터에 ID_300 매핑 #############################



