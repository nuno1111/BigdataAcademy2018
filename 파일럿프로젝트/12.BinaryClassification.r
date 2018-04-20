#################### 데이터로딩 ####################
dataset_poi_land_origin <- read.csv("./DATA/DATASET_POI_LAND.csv")
dataset_poi_land <- dataset_poi_land_origin

#################### 결측치 처리 ####################
# library(rgdal)
# lnd_gis <- readOGR(dsn = "./GIS", layer = "PCELL_ID_300") # GIS SHP 파일 로딩
# lnd_data <- lnd_gis@data
# 
# # ID_300 별 X,Y좌표만 추출하여 매핑
# t1 <- lapply(lnd_gis@polygons, function(x){x@Polygons[[1]]@coords[1,]})
# t2 <- unlist(t1) # vector
# t3 <- matrix(t2, ncol = 2, byrow = T) # matrix
# result <- as.data.frame(t3) # data.frame
# colnames(result) <- c("utmk_long","utmk_lat")
# 
# lnd_data <- cbind(lnd_data,result)
# 
# library(data.table)
# lnd_table <- data.table(lnd_data)
# lnd_table
# setkey("ID_300")
# 
# # 위경도 결합
# library(sqldf)
# dataset_poi_land <- sqldf("
#   SELECT A.*,B.utmk_long,B.utmk_lat
#   FROM dataset_poi_land A 
#   LEFT OUTER JOIN lnd_data B
#   ON A.ID_300 = B.ID_300
# ")
# 
# 
# head(dataset_poi_land)

# 결측치 메꾸려다가 어차피 데이터 많으니깐 결측치 제거..
dataset_poi_land <- dataset_poi_land[!is.na(dataset_poi_land$land_price),]
dataset_poi_land <- dataset_poi_land[!is.na(dataset_poi_land$land_type),]


#################### 데이터셋 생성 #################

qt <- quantile(dataset_poi_land$VALUE, c(.9)) # 상위 10% 지점

# y값 label 설정
dataset_poi_land$LABEL = 0
dataset_poi_land[dataset_poi_land$VALUE > qt,]$LABEL = 1

# factor 설정
dataset_poi_land$YYYYMM <- as.factor(dataset_poi_land$YYYYMM)
dataset_poi_land$HOUR <- as.factor(dataset_poi_land$HOUR)

# sampling 10%
set.seed(1234)  # seed 고정
# idx <- sample(1:nrow(dataset_poi_land), nrow(dataset_poi_land) * 0.1)
# dataset_poi_land_sample <- dataset_poi_land[idx, ]

dataset_poi_land_good = dataset_poi_land[dataset_poi_land$LABEL == 1,]
dataset_poi_land_bad = dataset_poi_land[dataset_poi_land$LABEL == 0,]

idx <- sample(1:nrow(dataset_poi_land_bad), nrow(dataset_poi_land_good))
dataset_poi_land_bad <- dataset_poi_land_bad[idx, ]
dataset_poi_land_sample <- rbind(dataset_poi_land_bad,dataset_poi_land_good)
# dataset생성
dataset <- dataset_poi_land_sample[,c(-1,-2,-7,-8,-9)]

#################### 7:3 ####################
idx <- sample(1:nrow(dataset), nrow(dataset)*0.7)
train <- dataset[idx, ]
test  <- dataset[-idx,]

#################### Logistic Regression ####################
log_model <- glm(factor(LABEL) ~ ., data = train, family = "binomial")
summary(log_model)

#################### Confusion Matrix ####################
pre <- predict(log_model,test[,-27],type="response")
head(pre)
cut <- ifelse(pre >= 0.5, 1, 0) ## 절단값 0.5로 설정
result <- table(test$LABEL,cut)

# 정확도
accuracy <- (result[1, 1] + result[2, 2]) / sum(result) * 100
accuracy

# 특이도
specificity <- result[2, 2] / (result[2, 1] + result[2, 2]) * 100
specificity

#################### ROC ####################

library(ROCR)
pr <- prediction(pre, test$LABEL)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# 0.932????? 오버피팅 된것인가??

# 전체데이터(100%)로 다시 한번

# dataset_all <- dataset_poi_land[,c(-1,-2,-7,-8,-9)]
# 
# pre_all <- predict(log_model,dataset_all[,-27],type="response")
# pr_all <- prediction(pre_all, dataset_all$LABEL)
# prf_all <- performance(pr_all, measure = "tpr", x.measure = "fpr")
# plot(prf_all)
# 
# auc_all <- performance(pr_all, measure = "auc")
# auc_all <- auc_all@y.values[[1]]
# auc_all
# 비슷하게 나오넹...

#################### SCORE ####################
test$score <- round(pre*1000,0)
# dataset_all$score <- round(pre_all*1000,0)

#################### K-S ####################
ks.test(test$score,test$LABEL)
# ks.test(dataset_all$score,dataset_all$LABEL)

# D = 0.85259 # 오버 피팅 된것인가?????

#################### K-S Chart ####################
# 우량 불량 나누기
sample1 <- test[test$LABEL==1,]$score # 우량
sample2 <- test[test$LABEL==0,]$score # 불량
# sample1 <- dataset_all[dataset_all$LABEL==1,]$score # 우량
# sample2 <- dataset_all[dataset_all$LABEL==0,]$score # 불량

group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)

# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 

# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ks_d_value <- (y1[1] - y0[1])
ks_d_value

library(ggplot2)
ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Sample") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
  ggtitle("K-S Test: Sample 1 / Sample 2") +
  theme(legend.title=element_blank())




