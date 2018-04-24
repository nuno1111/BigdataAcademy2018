##Logistic Regression

#===================================================================================
### 4-3. dataset_fw 데이터 모형적합(10% sampling)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_fw), nrow(dataset_fw)*0.9)
dataset_fw_1 <- dataset_fw[-idx,]

goodbad <- sqldf('
                 select label
                 ,   sum(1) as cnt    
                 from test
                 group by label
                 ')
# 가을/겨울 dataset의 불량 비중 : 12.6% (전체 불량 비중 : 10%)

## 학습데이타, 검정데이터 생성(7:3)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_fw_1), nrow(dataset_fw_1)*0.7)

train <- dataset_fw_1[idx, ]
test  <- dataset_fw_1[-idx,]
# 우불량 비율이 모집단과 비슷하여 학습,검정 Data로 활용

log_model <- glm(LABEL ~ ., data = train, family = "binomial")
round(coef(summary(log_model)), 3)
# 공원.산.동.식물원, 치안기관 변수는 p-value가 유의수준 5%에서 유의하지 않음


## 변수선택 Stepwise 활용 
new_model <- step(log_model, trace = F)
round(coef(summary(new_model)), 3)
# 공원.산.동.식물원, 치안기관 변수 삭제

## LR test
anova(new_model, log_model, test = "LRT")
# p-value가 유의수준 5%에서 유의하지 않으므로 모형의 차이가 없다는 귀무가설 채택.
# 즉, 새로운 모형을 채택


#===================================================================================
### 4-3. 모델평가
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model, newdata=test, type="response")
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
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.7934601 (전체 dataset AUC : 0.7898673, s/s AUC : 0.792944)


#===================================================================================
### 5-3. 모델평가(전체 dataset_fw 데이터 기준)
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model, newdata=dataset_fw, type="response")
pr <- prediction(p, dataset_fw$LABEL)
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
auc # 0.793488 (결과는 fair, 0.8에 근접한 우수한 모델로 평가)

# 학습, 검정데이터셋으로 평가한 AUC는 0.7934601
# new_model로 전체 데이터셋을 평가한 AUC는 0.793488