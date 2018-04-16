# 유동인구 데이터 로딩
# df_time_pivot <- read.csv('./DATA/DF_TIME_POVOT.csv')
# df_time_pivot <- df_time_pivot[df_time_pivot$YYYYMM != '201312',] # 2013/12 데이터 제거
# df_time_pivot$X <- NULL

# 날씨
df_time_clear_pivot <- read.csv('./DATA/DF_TIME_CLEAR_PIVOT.csv')
df_time_cloudy_pivot <- read.csv('./DATA/DF_TIME_CLOUDY_PIVOT.csv')
df_time_rain_pivot <- read.csv('./DATA/DF_TIME_RAIN_PIVOT.csv')
df_time_snow_pivot <- read.csv('./DATA/DF_TIME_SNOW_PIVOT.csv')

df_time_clear_pivot$WEATHER <- "CLEAR"
df_time_cloudy_pivot$WEATHER <- "CLOUDY"
df_time_rain_pivot$WEATHER <- "RAIN"
df_time_snow_pivot$WEATHER <- "SNOW"

df_time_weather <- rbind.data.frame(df_time_clear_pivot,df_time_cloudy_pivot,df_time_rain_pivot,df_time_snow_pivot) 

# 휴일여부
df_time_before_holiday_pivot <- read.csv('./DATA/DF_TIME_BEFORE_HOLIDAY_PIVOT.csv')
df_time_holiday_pivot <- read.csv('./DATA/DF_TIME_HOLIDAY_PIVOT.csv')
df_time_weekday_pivot <- read.csv('./DATA/DF_TIME_WEEKDAY_PIVOT.csv')

df_time_before_holiday_pivot$IS_HOLIDAY <- "BEFORE_HOLIDAY"
df_time_holiday_pivot$IS_HOLIDAY <- "HOLIDAY"
df_time_weekday_pivot$IS_HOLIDAY <- "WEEKDAY"

# 날씨 + 휴일
df_time <- sqldf("
  SELECT 
    A.ID_300,
    A.YYYYMM,
    A.variable AS HOUR,
    A.IS_HOLIDAY,
    B.WEATHER,
    A.value AS HOLIDAY_VALUE,
    B.value AS WEATHER_VALUE
  FROM
  (
    SELECT * FROM df_time_before_holiday_pivot
    UNION ALL
    SELECT * FROM df_time_holiday_pivot
    UNION ALL
    SELECT * FROM df_time_weekday_pivot
  ) A 
  JOIN 
  (
    SELECT * FROM df_time_clear_pivot
    UNION ALL
    SELECT * FROM df_time_cloudy_pivot
    UNION ALL
    SELECT * FROM df_time_rain_pivot
    UNION ALL
    SELECT * FROM df_time_snow_pivot
  ) B
  ON A.ID_300 = B.ID_300 AND A.YYYYMM = B.YYYYMM AND A.variable = B.variable 
")

# 날씨 + 휴일은 구할 수가 없으므로 ex(맑은 날씨의 휴일) 평균값으로 계산
# write.csv(df_time,"./DATA/DF_TIME.csv")

df_time <- df_time[df_time$YYYYMM != '201312',] # 2013/12 데이터 제거
df_time$VALUE <- (df_time$HOLIDAY_VALUE + df_time$WEATHER_VALUE)/2

# POI데이터 로딩
poi_with_id300 <- read.csv('./DATA/POI_WITH_ID300.csv')
poi_with_id300$X.1 <- NULL

# POI 데이터 ID_300기준 정렬
library(sqldf)
id300_type_count <- sqldf("
  SELECT ID_300, TYPE, SUM(CNT) COUNT FROM (
    SELECT ID_300, TYPE, 1 AS CNT FROM poi_with_id300
  ) GROUP BY ID_300, TYPE
  ORDER BY COUNT DESC
")

# 역pivot
library(reshape)
id300_type_pivot <- cast(id300_type_count, ID_300 ~ TYPE) # cast 해서 역 pivot
id300_type_pivot[is.na(id300_type_pivot)] <- 0 # 결측치 0으로 대체
  
# 데이터 결합 (df_time + poi)
dataset = merge(x=df_time,
                y=id300_type_pivot,
                by='ID_300',
                all.x=TRUE)

dataset[is.na(dataset)] <- 0 # 결측치 0으로 대체

# write.csv(dataset,"./DATA/DATASET.csv")
# write.csv(id300_type_pivot,"./DATA/ID_300_TYPE_PIVOT.csv")















