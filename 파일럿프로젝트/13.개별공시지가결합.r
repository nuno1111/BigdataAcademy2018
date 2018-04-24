#### 개별공시지가 정보 로딩 ####
# rm(list = ls());gc();
library(rgdal)
lnd <- readOGR(dsn = "./DATA/AL_50", layer = "AL_50_D150_20160921", encoding = "euc-kr") # GIS SHP 파일 로딩

#### 개별공시지가 데이터셋 생성 ####

t1 <- lapply(lnd@polygons, function(x){x@Polygons[[1]]@labpt})
t2 <- unlist(t1) # vector
t3 <- matrix(t2, ncol = 2, byrow = T) # matrix
t <- as.data.frame(t3) # data.frame
colnames(t) <- c("long","lat")

land_type <- lnd$A12
land_price <- lnd$A13

dataset <- data.frame(t,land_type,land_price)
dataset <- dataset[!is.na(dataset$land_type),] # 결측치 제거

#### 좌표변환 #####
library(sp)

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

coord <- data.frame(long=dataset$long, lat=dataset$lat) # 위경도 데이터 추출

# 개별공시지가 좌표계
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs"

# 300 그리드에서 사용된 좌표계 utmk
to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs" 

# 좌표계 변환 수행 후 컬럼 추가
coord <- cbind(coord, convertCoordSystem(coord$long, coord$lat, from.crs, to.crs))
dataset <- cbind(dataset, coord[,c("utmk_long","utmk_lat")])

############################# 300 GRID 격자에서 ID별 위경도 추출 #############################
lnd_gis <- readOGR(dsn = "./GIS", layer = "PCELL_ID_300") # GIS SHP 파일 로딩
lnd_data <- lnd_gis@data

# ID_300 별 X,Y좌표만 추출하여 매핑
t1 <- lapply(lnd_gis@polygons, function(x){x@Polygons[[1]]@coords[1,]})
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

# write.csv(dataset,"./DATA/DATASET_LAND.csv")

dataset_id <- sqldf("
      SELECT 
        A.*,B.ID_300 
      FROM dataset A 
      LEFT OUTER JOIN lnd_table B
      ON A.utmk_long >= B.utmk_long 
      AND A.utmk_long < B.utmk_long+300 
      AND A.utmk_lat >= B.utmk_lat
      AND A.utmk_lat < B.utmk_lat+300
    ")

#### 결측치 채우기 ####
dataset_id = read.csv("./DATA/DATASET_POI_FOR_IMPUTE.csv")
library(DMwR)
tmp <- dataset_id[,-1]

anyNA(tmp)
knnOutput <- knnImputation(tmp)  # perform knn imputation.
anyNA(knnOutput)

dataset_id[is.na(dataset_id$land_price),]$land_price <- knnOutput[is.na(dataset_id$land_price),]$land_price
dataset_id[is.na(dataset_id$land_price),]$land_type <- knnOutput[is.na(dataset_id$land_price),]$land_type
anyNA(dataset_id)

# write.csv(dataset_id,"./DATA/DATASET_LAND_IMPUTE.csv")

#### 기존데이터 JOIN ####
dataset_poi <- read.csv("./DATA/DATASET_POI.csv")

# dataset_poi_land <- sqldf("
#   SELECT 
#     A.*, 
#     B.land_price,
#     B.land_type
#   FROM dataset_poi A
#   LEFT OUTER JOIN 
#   (
#     SELECT ID_300, MAX(land_type) as LAND_TYPE, AVG(land_price) as LAND_PRICE FROM dataset_id GROUP BY ID_300
#   )B
#   ON A.ID_300 = B.ID_300
# ")

dataset_poi_land <- sqldf("
  SELECT
    A.*,
    B.land_price,
    B.land_type
  FROM dataset_poi A
  LEFT OUTER JOIN dataset_idB
  ON A.ID_300 = B.ID_300
")
