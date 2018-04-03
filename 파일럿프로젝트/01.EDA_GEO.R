# Objective
# 여행하며 즐기는 푸드트럭~~ 도전기~~
# 딱 한달만 제주도 여행과 푸드트럭 운영을 위한 최적의 장소/메뉴/일정을 데이터 분석을 통해 정하는 것을 목표

# ex) 매일 다른 곳을 이동하며 푸드트럭 장사를 한다.
# ex) 일정 수준 이상의 인원이 없으면 장사를 접고 여행에 집중한다.
# ex) 

# 대상 Target Top 3
# 시간 Target Top 3 : 시간대/계절/월
# 장소 Target Top 3

# 데이터 로딩 ----
df_time <- read.csv("./TIME/TIME_300.csv")

# 데이터 탐색 
head(df_time)
str(df_time)
summary(df_time)

# 컬럼명 변경
colnames(df_time) <- c(
  "YYYYMM"
  ,"ID_300"
  ,"X00_01"
  ,"X01_02"
  ,"X02_03"
  ,"X03_04"
  ,"X04_05"
  ,"X05_06"
  ,"X06_07"
  ,"X07_08"
  ,"X08_09"
  ,"X09_10"
  ,"X10_11"
  ,"X11_12"
  ,"X12_13"
  ,"X13_14"
  ,"X14_15"
  ,"X15_16"
  ,"X16_17"
  ,"X17_18"
  ,"X18_19"
  ,"X19_20"
  ,"X20_21"
  ,"X21_22"
  ,"X22_23"
  ,"X23_00"
  ,"X00_01_RATIO"
  ,"X01_02_RATIO"
  ,"X02_03_RATIO"
  ,"X03_04_RATIO"
  ,"X04_05_RATIO"
  ,"X05_06_RATIO"
  ,"X06_07_RATIO"
  ,"X07_08_RATIO"
  ,"X08_09_RATIO"
  ,"X09_10_RATIO"
  ,"X10_11_RATIO"
  ,"X11_12_RATIO"
  ,"X12_13_RATIO"
  ,"X13_14_RATIO"
  ,"X14_15_RATIO"
  ,"X15_16_RATIO"
  ,"X16_17_RATIO"
  ,"X17_18_RATIO"
  ,"X18_19_RATIO"
  ,"X19_20_RATIO"
  ,"X20_21_RATIO"
  ,"X21_22_RATIO"
  ,"X22_23_RATIO"
  ,"X23_00_RATIO"
)

head(df_time)
str(df_time)
summary(df_time)

colnames_selected <- c(
  "YYYYMM"
  ,"ID_300"
  ,"X00_01"
  ,"X01_02"
  ,"X02_03"
  ,"X03_04"
  ,"X04_05"
  ,"X05_06"
  ,"X06_07"
  ,"X07_08"
  ,"X08_09"
  ,"X09_10"
  ,"X10_11"
  ,"X11_12"
  ,"X12_13"
  ,"X13_14"
  ,"X14_15"
  ,"X15_16"
  ,"X16_17"
  ,"X17_18"
  ,"X18_19"
  ,"X19_20"
  ,"X20_21"
  ,"X21_22"
  ,"X22_23"
  ,"X23_00"
)
df_time <- df_time[df_time$ID_300 > 1000020000,] # 추자도 제거하고 제주도만
df_time <- df_time[,colnames_selected] # 비율데이터는 제거

head(df_time)
str(df_time)
summary(df_time)

# GIS 처리  ----
library(rgdal)
lnd <- readOGR(dsn = "./GIS", layer = "PCELL_ID_300") # GIS SHP 파일 로딩

lnd_target <- lnd@data[lnd$ID_300 %in% df_time$ID_300,] # 데이터 있는 격자 찾기

plot(lnd[lnd_target,]) # 데이터가 있는 격자만 그리기

# 데이터 결합
# install.packages("sqldf")
library(sqldf)
df_time_201401 <- sqldf("SELECT * FROM df_time WHERE YYYYMM = '201401'")
lnd_data <- lnd@data
lnd@data <- sqldf("SELECT B.* FROM lnd_data A LEFT OUTER JOIN df_time_201401 B ON A.ID_300 = B.ID_300  ")

lnd <- lnd[!is.na(lnd$ID_300),]

# 데이터 시각화
library(tmap) # load tmap package (see Section IV)
lnd$X12_13_LOG <- log(lnd$X12_13+1)
qtm(lnd, "X12_13_LOG") # plot the basic map
