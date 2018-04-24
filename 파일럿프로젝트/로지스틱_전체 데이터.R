##Logistic Regression

# DataSet Loading
dataset_poi <- read.csv("./DATASET_POI.csv")

head(dataset_poi, 1)
str(dataset_poi)

#===================================================================================
### 1. 개발요건 정의
# 명절과 날씨의 영향이 종속변수에 영향이 있을 수 있다는 가정하에 
# 2개의 신용평가모형을 만들어 보고 전체 dataset으로 만든 모델과 성능 비교
# 첫번째 dataset : 2014.04 ~ 09 (6개월)
# 두번째 dataset : 2014.10 ~ 12, 2014.01 ~ 2014.03(6개월)
#===================================================================================


#===================================================================================
### 2. 우량/불량 정의
# 종속변수(y) 만들기
# 신용평가에서 불량이란 더이상 납부할 의사가 없는 고객으로 정의하며
# 판단미정은 미납이 있으나 불량이 아닌고객으로 정의함
# 우량은 불량과 판단미정을 제외한 고객
# 이 프로젝트에서는 홀리데이와 날씨 value가 반영된 value 값이 
# 높은 곳을 우량으로하여 우불량 비율을 9:1로 설정하였음(판단미정 없음)


## 종속변수(y) 만들기
# 유동인구가 낮으면(전체 데이터 중 VALUE 기준 하위 10%) 불량으로 정의
qt <- quantile(dataset_poi$VALUE, c(.10))
qt <- c(-Inf, qt, Inf)
dataset_poi$LABEL <- cut(dataset_poi$VALUE, breaks = qt, labels = c(0,1)) 
# VALUE를 이항변수로 변경(1 : 우량, 0 : 불량)


dataset_poi$LABEL <- as.factor(dataset_poi$LABEL)
# 로지스틱 회귀모형을 적용하기 위해 종속변수 형 변환

#library(sqldf)
goodbad <- sqldf('
                 select time
                    ,   sum(VALUE) as cnt    
                 from dataset_poi
                 group by time
                ')
#library(dplyr)
dataset_poi %>% filter(LABEL == '1')

head(dataset_poi)


#===================================================================================
### 3. 자료추출
## 독립변수 만들기(기존변수HOUR → time)
# dataset_poi$HOUR <- as.integer(dataset_poi$HOUR)
# 5 ~ 12시 : 오전
# 13시 ~ 20시 : 오후
# 21시 ~ 4시 : 밤
dataset_poi$time = factor("밤",c("오전","오후","밤"))
dataset_poi[dataset_poi$HOUR >= 5 & dataset_poi$HOUR <= 12,]$time = "오전"
dataset_poi[dataset_poi$HOUR >= 13 & dataset_poi$HOUR <= 20,]$time = "오후"

## 더미변수 만들기
# VALUE의 값이 날씨와 휴일여부에 의하여 만들어졌기 때문에 영향력이 높을 것으로 예상
# 범주형 변수를 버리지 않고 더미변수로 생성
# 기준이 되는 값은 보통 일반적이거나 빈도수가 많은 범주로 선택
head(dataset_poi,1)
# 기존변수 HOUR
dataset_poi <- transform(dataset_poi,
                         TM_1 = ifelse(time  == '오후', 1, 0),
                         TM_2 = ifelse(time  == '오전', 1, 0)) # 밤은 0,0

# 기존변수 WEATHER
dataset_poi <- transform(dataset_poi,
                         WH_1 = ifelse(WEATHER == 'CLEAR',  1, 0),
                         WH_2 = ifelse(WEATHER == 'CLOUDY', 1, 0),
                         WH_3 = ifelse(WEATHER == 'RAIN',   1, 0)) # SNOW는 0, 0, 0

# 기존변수 IS_HOLIDAY
dataset_poi <- transform(dataset_poi,
                         HD_1 = ifelse(IS_HOLIDAY  == 'WEEKDAY', 1, 0),
                         HD_2 = ifelse(IS_HOLIDAY  == 'HOLIDAY', 1, 0)) # BEFORE_HOLIDAY는 0,0

str(dataset_poi)
head(dataset_poi,1)

## dataset 만들기
# dataset_ss : 2014.04 ~ 09 (6개월)
# dataset_fw : 2014.10 ~ 12, 2014.01 ~ 2014.03(6개월)
dataset_poi$YYYYMM <- as.factor(dataset_poi$YYYYMM)
dataset_ss <- subset(dataset_poi, YYYYMM %in% c("201404", "201405","201406","201407","201408","201409"))
dataset_fw <- subset(dataset_poi, YYYYMM %in% c("201401", "201402","201403","201410","201411","201412"))


# 정제(원본, Spring/Summer, Fall/Winter) 필요없는 값 버리기
dataset_all <- dataset_poi[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-31)] # row데이터는 놔두고..
dataset_ss <- dataset_ss[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-31)]
dataset_fw <- dataset_fw[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-31)]

head(dataset_all,1)
head(dataset_ss,1)
head(dataset_fw,1)

#===================================================================================
### 4-1. 모형적합(전체 data 중 10% sampling)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_all), nrow(dataset_all)*0.9)
dataset_all_1 <- dataset_all[-idx,]

goodbad <- sqldf('
                 select label
                    ,   sum(1) as cnt    
                 from train
                 group by label
                ')
# 우불량 비율이 9:1로 모집단 특성이 잘 반영되어 Sampling Data로 활용

## 학습데이타, 검정데이터 생성(7:3)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_all_1), nrow(dataset_all_1)*0.7)

train <- dataset_all_1[idx, ]
test  <- dataset_all_1[-idx,]
# 우불량 비율이 9:1로 모집단 특성이 잘 반영되어 학습,검정 Data로 활용

log_model_all <- glm(LABEL ~ ., data = train, family = "binomial")
round(coef(summary(log_model_all)), 3)
# 공원.산.동.식물원 변수는 p-value가 0.856으로 유의수준 5%에서 유의하지 않음


## 변수선택 Stepwise 활용 
new_model_all <- step(log_model_all, trace = F)

## LR test
anova(new_model_all, log_model_all, test = "LRT")
# p-value가 유의수준 5%에서 유의하지 않으므로 모형의 차이가 없다는 귀무가설 채택.
# 즉, 새로운 모형을 채택


#anova(new_model_all, test="Chisq")
# 레져.스포츠 변수의 Pr(>Chi) 값이 유의수준 보다 큰나 stepwise에서 제거 되지 않음
# Estimate 값이 높지않아 해당 모델에서는 포함시켰음



#===================================================================================
### 4-1. 모델평가
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model_all, newdata=test, type="response")
pr <- prediction(p, test$LABEL)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# train dataset으로 만든 모델을 test dataset에 적용했을 경우의 ROC 곡선

## AUC 넓이 (ROC 그래프 아래 영역)
# excellent : 0.9 ~ 1
# good : 0.8 ~ 0.9
# fair : 0.7 ~ 0.8
# poor : 0.6 ~ 0.7
# fail : 0.5 ~ 0.6
# 모든 case의 결과를 완벽하게 예측하는 모델인 경우 AUC는 1
# 무작위로 예측한 모델과 다를 바가 없는 경우 AUC는 0.5의 값을 가짐
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc 
# 0.7898673 (결과는 fair, 0.8에 근접한 우수한 모델로 평가)


#===================================================================================
### 5-1. 모델평가(전체 데이터 기준)
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model_all, newdata=dataset_all, type="response")
pr <- prediction(p, dataset_all$LABEL)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# train dataset으로 만든 모델을 test dataset에 적용했을 경우의 ROC 곡선

## AUC 넓이 (ROC 그래프 아래 영역)
# excellent : 0.9 ~ 1
# good : 0.8 ~ 0.9
# fair : 0.7 ~ 0.8
# poor : 0.6 ~ 0.7
# fail : 0.5 ~ 0.6
# 모든 case의 결과를 완벽하게 예측하는 모델인 경우 AUC는 1
# 무작위로 예측한 모델과 다를 바가 없는 경우 AUC는 0.5의 값을 가짐
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.7898673 (결과는 fair, 0.8에 근접한 우수한 모델로 평가)

# 학습, 검정데이터셋으로 평가한 AUC는 0.7898673
# new_model로 전체 데이터셋을 평가한 AUC는 0.7744534