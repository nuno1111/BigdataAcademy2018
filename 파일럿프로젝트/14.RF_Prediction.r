#install.packages("dplyr")
library(dplyr)
#rm(list = ls())
# df <- read.csv("./DATA/DATASET_POI_LAND_IMPUTED.csv",fileEncoding = "utf-8")
df <- read.csv("~/DATASET_POI_LAND_IMPUTED_2.csv",fileEncoding = "utf-8", stringsAsFactors = FALSE)


# 연월, ID를 범주형 변수로 처리합니다.
df$YYYYMM <- as.factor(df$YYYYMM) 
df$ID_300 <- as.factor(df$ID_300) 
df[is.na(df$land_type),]$land_type = ""
df$land_type <- as.factor(df$land_type)


# 데이터셋 축소하기 신공
# 1. 시각을 9 ~ 18 시로 제한

df_opt <- df %>% 
  filter(HOUR >= 9 & HOUR <= 18) %>% 
  select(ID_300, YYYYMM, HOUR, VALUE, IS_HOLIDAY, WEATHER,
         공공기관,
         공원.산.동.식물원,
         관광.숙박,
         교육기관,
         교통시설,
         금융기관,
         기업,
         농공시설,
         도로시설,
         레져.스포츠,
         문화.종교.예술,
         쇼핑.편의,
         언론기관,
         음식점,
         의료.복지,
         자동차관련,
         주택관련,
         지명관련,
         치안기관,
         land_price,
         land_type) %>% 
  arrange(ID_300, YYYYMM, HOUR)

# 밑에 쿼리문 돌릴때 신택스 에러를 방지하기 위해 영문으로 리네임
col_names <- c("ID","YM","HOUR","VALUE","DAY","WEATHER", "PUBLICS","PARK","SIGHT_HOTEL",
               "EDU","TRAFFIC","FIN","COM","FARM","ROAD","SPORTS","CUL",
               "SHOP","MEDIA","FOOD","MEDI","CARS","HOUSE","PLACE","POLICE","landprice","landtype")

colnames(df_opt) <- col_names # 변수명 변경

#install.packages("sqldf")
library(sqldf)

# 휴일, 날씨를 제외할거기때문에 다시 쿼리 날려서 그룹바이~ 합치기
df_opt2 <- sqldf("
                 SELECT 
                 ID,
                 substr(YM,5,2) as YM,
                 HOUR,
                 DAY, WEATHER,
                 sum(VALUE) as VALUE,
                 avg(PUBLICS) AS PUBLICS,
                 avg(PARK) AS PARK,
                 avg(SIGHT_HOTEL) AS SIGHT_HOTEL,
                 avg(EDU) AS EDU,
                 avg(TRAFFIC) AS TRAFFIC,
                 avg(FIN) AS FIN,
                 avg(COM) AS COM,
                 avg(FARM) AS FARM,
                 avg(ROAD) AS ROAD,
                 avg(SPORTS) AS SPORTS,
                 avg(CUL) AS CUL,
                 avg(SHOP) AS SHOP,
                 avg(MEDIA) AS MEDIA,
                 avg(FOOD) AS FOOD,
                 avg(MEDI) AS MEDI,
                 avg(CARS) AS CARS,
                 avg(HOUSE) AS HOUSE,
                 avg(PLACE) AS PLACE,
                 avg(POLICE) AS POLICE
                 FROM df_opt
                 group by ID, YM, HOUR, DAY, WEATHER
                 ") 
head(df_opt2)


# 지가정보는 ID별로 따로 묶어둡니다. ID를 필터링한 후 조인해서 보면
# 결측치 여부를 더 정확히 파악할 수 있을겁니다.

df_land <- sqldf("
                 select
                 ID, landtype,
                 avg(landprice) as landprice
                 from df_opt
                 group by ID, landtype
                 ")
head(df_land)


df_opt3 <- df_opt2

head(df_opt3)
dim(df_opt3)

df_temp <- sqldf("select
                 WEATHER as W,
                 avg(VALUE) as V
                 from df_opt3
                 group by W")
library(ggplot2)
ggplot(df_temp, aes(x=W, y=V)) + geom_bar(stat="identity",fill = "lightblue")
+ theme_classic()


# BEFORE_HOLIDAY = 1, HOLIDAY = 2, WEEKDAY = 3
df_opt3 <- transform(df_opt3, DAY = ifelse(DAY == "BEFORE_HOLIDAY", 1,
                                           ifelse(DAY == "HOLIDAY",2,3)))
df_opt3$DAY <- as.factor(df_opt3$DAY) 

unique(df_opt3$DAY)
# SNOW = 1, CLOUDY = 2, RAIN = 3, CLEAR = 4
df_opt3 <- transform(df_opt3, WEATHER = ifelse(WEATHER == "SNOW",1,
                                               ifelse(WEATHER == "CLOUDY",2,
                                                      ifelse(WEATHER == "RAIN",3,4))))
df_opt3$WEATHER <- as.factor(df_opt3$WEATHER) 

head(df_opt3)
summary(df_opt2)
unique(df_opt3$WEATHER)

#rm(df_rank) # 메모리 압박으로 자주 지워줘야만.. ㅠㅠ

# 월별, 시간별 모듈을 쪼개고 랭크 및 등급 부여후 합치기 위해
# 빈 프레임을 생성합니다.
df_rank = data.frame()

# 순환순 4라운드를 돌리기 위해 범위값 지정 
ym_value = unique(df_opt3$YM)
h_value = unique(df_opt3$HOUR)
d_value = unique(df_opt3$DAY)


# 여기서 부터 월별, 시간별 모듈쪼개서 유동인구로 랭크 변수 생성하고
# 100위까지만 잘라서 데이터를 줄인다
# 등급은 별도로 순환문을 돌려서 부여한다
for(x in ym_value) {
  for(y in h_value){
    for(d in d_value){
      df_temp <- df_opt3 %>% filter(YM == x & HOUR == y & DAY == d)
      
      # snow는 겨울에만 해당되기 때문에 여기서 범위설정
      w_value = unique(df_temp$WEATHER)
      df_temp4 = data.frame()
      for(w in w_value){
        df_temp2 <- df_temp %>% filter(WEATHER == w)
        
        # rank는 월,시간,휴일,날짜별로 부여
        df_temp2$RANK <- rank(-df_temp2$VALUE)
        df_temp3 <- df_temp2 %>% filter(RANK <= 101)
        df_temp4 <- rbind(df_temp4, df_temp3)    
      } 
      # Score는 월,시간,휴일별로만 부여 : 날씨의 패널티를 주기 위함
      qt <- quantile(df_temp4$VALUE, c(.125,.250,.375,.5,.625,.750,.875))
      qt <- c(0,qt,Inf)
      df_temp4$LABEL <- cut(df_temp4$VALUE, breaks = qt, labels = c(1:8))
      df_rank <- rbind(df_rank, df_temp4)
    }
  }
}
head(df_rank)
# 그닥 멋지지 않은 랭크별 아뒤 분포 보기 --;
summary(df_rank %>% filter(RANK == 1))

# 유동인구가 압도적으로 많은 제주시는 제외합니다. 1000031709

df_rank <- df_rank %>% filter(RANK > 1)


head(df_rank)
summary(df_rank)


# 이 상태에서 지가를 조인해보겠습니다.

df_rank <- sqldf("
                 select a.*, 
                 b.landprice
                 from df_rank a
                 left outer join df_land b
                 on a.ID = b.ID
                 ")

table(is.na(df_rank$landprice))

df_rank$DAY <- as.factor(df_rank$DAY) 
df_rank$WEATHER <- as.factor(df_rank$WEATHER) 

# 4월 21일 모임에서 범주형 변수 고려사항
#  1. 월 ->  봄(3~5),여름(6~9),가을(10~11), 겨울(12~2) 로 그룹핑한 변수 추가
#     9월은 사실상 여름이나 마찬가지라 여름으로 포함시켰습니다.
#     주관에 따라 그룹핑 조건은 달리 부여해도 될듯 합니다.
#  2. 시간 -> 오전(9~12), 오후(13~18) 

df_rank <- df_rank %>% 
  mutate(season = ifelse(YM == c("03","04","05"),1, 
                         ifelse(YM == c("06","07","08","09"),2,
                                ifelse(YM == c("10","11"),3,4))))
df_rank <- df_rank %>% 
  mutate(ampm=ifelse(HOUR <13, 1, 2))


df_rank$season <- as.factor(df_rank$season) 
df_rank$ampm <- as.factor(df_rank$ampm)

############# 4/21 오전/오후, 계절 변수 추가 완료




### 히스토그램 (Histogram) : 등급별로 고루 분포되었더라구요
#install.packages("ggplot2")

library(ggplot2)
ggplot(df_rank, aes(LABEL)) + geom_bar(fill = "lightblue")
+ theme_classic()

# 유니크한 ID를 생성해줬습니다. 월, 시간을 붙여줬어요
df_rank$ID_NEW <- paste(df_rank$ID,"_",df_rank$YM,"_",df_rank$HOUR)

head(df_rank)

# 피쳐변수명을 정의했습니다. 제외한 놈들은 없습니다

features_names <- c("YM","HOUR","DAY", "WEATHER", "landprice","PUBLICS","PARK","SIGHT_HOTEL",
                    "EDU","TRAFFIC","FIN","COM","FARM","ROAD","SPORTS","CUL",
                    "SHOP","MEDIA","FOOD","MEDI","CARS","HOUSE","PLACE","POLICE")
label_name <- "LABEL"

ntree = 500
mtry = 15


## 랜덤포레스트 함수 
#install.packages("randomForest")
library(randomForest)

# RF <- function(dataset, features){
# RF(df_rank,features_names)   # 86.2%

dataset <- df_rank
features <- features_names

df_set <- dataset %>% select(features, label_name)
set.seed(123)

dim <- dim(df_set)
#train/ test 나누기
sample.num  <- sample(1:nrow(df_set), 0.8*nrow(df_set))
train       <- df_set[sample.num,]
test        <- df_set[-sample.num,]
x.train <- train[,-dim[2]]
x.test <- test[,-dim[2]]
y.train <- train[,dim[2]]
y.test <- test[,dim[2]]

#Random Forest
fit.rf <- randomForest(as.factor(LABEL)~.,data=train, ntree=ntree, mtry=mtry)
#예측
pred<-predict(fit.rf,x.test)

print(table(pred,y.test))

# 인식율
mean(pred == y.test)


################################### 여기까지 기존코드 ###################################
#### 예측 ####

last <- read.csv("./DATA/DATASET_LAST_2.csv",fileEncoding = "utf-8", stringsAsFactors = FALSE)

# substr(YM,5,2) as YM,
last$YM <- sapply(last$YM, toString)
last$YM <- substr(last$YM,5,6)

# BEFORE_HOLIDAY = 1, HOLIDAY = 2, WEEKDAY = 3
last <- transform(last, DAY = ifelse(DAY == "BEFORE_HOLIDAY", 1,
                                           ifelse(DAY == "HOLIDAY",2,3)))
last$DAY <- as.factor(last$DAY) 

# SNOW = 1, CLOUDY = 2, RAIN = 3, CLEAR = 4
last <- transform(last, WEATHER = ifelse(WEATHER == "SNOW",1,
                                               ifelse(WEATHER == "CLOUDY",2,
                                                      ifelse(WEATHER == "RAIN",3,4))))
last$WEATHER <- as.factor(last$WEATHER)

last[is.na(last$land_type),]$land_type = ""
last$land_type <- as.factor(last$land_type)

#예측
pred_last<-predict(fit.rf,last)
result <- last

result$PREDICT <- pred_last
write.csv(result, "~/RF_RESULT_2.csv", row.names = FALSE)

