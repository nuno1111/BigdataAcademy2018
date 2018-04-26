#################### 데이터로딩 ####################
# dataset_poi_land_origin <- read.csv("./DATA/DATASET_POI_LAND_IMPUTED.csv")
dataset_poi_land_origin <- read.csv("~/DATASET_POI_LAND_IMPUTED_2.csv", stringsAsFactors = FALSE)
dataset_poi_land <- dataset_poi_land_origin 

#################### 데이터셋 생성 #################

qt <- quantile(dataset_poi_land$VALUE, c(.9)) # 상위 10% 지점

# y값 label 설정
dataset_poi_land$LABEL = 0
dataset_poi_land[dataset_poi_land$VALUE > qt,]$LABEL = 1

# factor 설정
dataset_poi_land$YYYYMM <- as.factor(dataset_poi_land$YYYYMM)
dataset_poi_land$HOUR <- as.factor(dataset_poi_land$HOUR)
dataset_poi_land[is.na(dataset_poi_land$land_type),]$land_type = ""
dataset_poi_land$land_type <- as.factor(dataset_poi_land$land_type)


# sampling 10%
set.seed(1234)  # seed 고정
# idx <- sample(1:nrow(dataset_poi_land), nrow(dataset_poi_land) * 0.1)
# dataset_poi_land_sample <- dataset_poi_land[idx, ]

# 우량은 전부다 불량 1/9로 1:1로 데이터 맞추어서..
dataset_poi_land_good = dataset_poi_land[dataset_poi_land$LABEL == 1,]
dataset_poi_land_bad = dataset_poi_land[dataset_poi_land$LABEL == 0,]

idx <- sample(1:nrow(dataset_poi_land_bad), nrow(dataset_poi_land_good))
dataset_poi_land_bad <- dataset_poi_land_bad[idx, ]
dataset_poi_land_sample <- rbind(dataset_poi_land_bad,dataset_poi_land_good)
# dataset생성
dataset <- dataset_poi_land_sample[,c(-1,-2,-7,-8,-9)]

# write.csv(dataset, "/home/nuno1026/DATASET_BINARYCLASSFICATION.csv", row.names=FALSE)

#################### 7:3 ####################
idx <- sample(1:nrow(dataset), nrow(dataset)*0.7)
train <- dataset[idx, ]
test  <- dataset[-idx,]

#################### Logistic Regression ####################
log_model <- glm(factor(LABEL) ~ ., data = train, family = "binomial")

################################### 여기까지 기존코드 ###################################
#### 예측 ####

last24 <- read.csv("~/DATASET_LAST_24_2.csv",fileEncoding = "utf-8", stringsAsFactors = FALSE)
last24_temp <- last24

colnames(last24_temp) <- c("YYYYMM","HOUR","IS_HOLIDAY","WEATHER","ID_300","공공기관","공사중명칭",
                       "공원.산.동.식물원","관광.숙박","교육기관","교통시설","금융기관","기업","농공시설",
                       "도로시설","레져.스포츠","문화.종교.예술","쇼핑.편의","언론기관",
                       "음식점","의료.복지","자동차관련","주택관련","지명관련","치안기관",         
                       "land_price","land_type")

last24_temp$YYYYMM <- as.factor(last24_temp$YYYYMM)
last24_temp$HOUR <- as.factor(last24_temp$HOUR)
last24_temp[is.na(last24_temp$land_type),]$land_type = ""
last24_temp$land_type <- as.factor(last24_temp$land_type)

pre_all <- predict(log_model,last24_temp,type="response")

#################### SCORE ####################
last24_temp$score <- round(pre_all*1000,0)

# 명목형 y값 만들기
qt <- quantile(last24_temp$score,  c(.125,.250,.375,.5,.625,.750,.875))
qt <- c(-Inf,qt,Inf)
last24_temp$GRADE <- cut(last24_temp$score, breaks = qt, labels = c(1:8)) # VALUE를 명목형 변수로 변경
write.csv(last24_temp, "~/LR_RESULT_2.csv", row.names = FALSE)

