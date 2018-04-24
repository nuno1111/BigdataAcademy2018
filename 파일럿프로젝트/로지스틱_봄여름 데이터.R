##Logistic Regression

#===================================================================================
### 4-2. dataset_ss 데이터 모형적합(10% sampling)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_ss), nrow(dataset_ss)*0.9)
dataset_ss_1 <- dataset_ss[-idx,]

goodbad <- sqldf('
                 select label
                 ,   sum(1) as cnt    
                 from train
                 group by label
                 ')
# 봄/여름 dataset의 불량 비중 : 7.5% (전체 불량 비중 : 10%)

## 학습데이타, 검정데이터 생성(7:3)
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_ss_1), nrow(dataset_ss_1)*0.7)

train <- dataset_ss_1[idx, ]
test  <- dataset_ss_1[-idx,]
# 우불량 비율이 모집단과 비슷하여 학습,검정 Data로 활용

log_model_ss <- glm(LABEL ~ ., data = train, family = "binomial")
round(coef(summary(log_model_ss)), 3)
# 언론기관, WH_1 변수는 p-value가 유의수준 5%에서 유의하지 않음


## 변수선택 Stepwise 활용 
new_model_ss <- step(log_model_ss, trace = F)
round(coef(summary(new_model_ss)), 3)
# WH_1 만 삭제됨

## LR test
anova(new_model_ss, log_model_ss, test = "LRT")
# p-value가 유의수준 5%에서 유의하지 않으므로 모형의 차이가 없다는 귀무가설 채택.
# 즉, 새로운 모형을 채택


#===================================================================================
### 4-2. 모델평가
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model_ss, newdata=test, type="response")
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
auc # 0.792944 (전체 dataset AUC : 0.7898673)


#===================================================================================
### 5-2. 모델평가(전체 dataset_ss 데이터 기준)
## ROC 곡선
# ROC 커브 아래의 영역 AUC가 1에 가까울 수록 모델의 예측 정확도가 높음

#install.packages("ROCR")
library(ROCR)
p <- predict(new_model_ss, newdata=dataset_ss, type="response")
pr <- prediction(p, dataset_ss$LABEL)
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
auc # 0.7934569

# 학습, 검정데이터셋으로 평가한 AUC는 0.792944
# new_model로 전체 데이터셋을 평가한 AUC는 0.7934569


# 신용평가점수(등급)을 만들기 때문에 cut-off가 필요없음


#===================================================================================
### 6. 평가표 작성(전체 dataset_ss 데이터 기준)
dataset_ss$pre <- round((predict(new_model_ss, newdata=dataset_ss, type="response"))*1000,0)


#install.packages("ggplot2")
library(ggplot2)


## 점수별 히스토그램
ggplot(dataset_ss, aes(dataset_ss$pre)) +
  geom_histogram(fill="blue", alpha=0.4, color="black", bins=10) +
  theme_bw() +
  labs(x = "Score", y="Frequency") +
  ggtitle("Histogram of Score") +
  theme(plot.title=element_text(face="bold", hjust=0.5, size=14, color="blue"))
  

library(sqldf)
a1      <- sqldf('
                 select case when pre < 750 then "5등급"
                             when pre < 900 then "4등급"
                             when pre < 970 then "3등급"
                             when pre <= 999 then "2등급"
                 ,   count(1) as cnt    
                 from dataset_ss
                 group by case when pre < 750 then "5등급"
                               when pre < 900 then "4등급"
                               when pre < 970 then "3등급"
                               when pre <= 999 then "2등급"
                               else "1등급" end
                 order by 1
                 ')
a1


## 등급화
dataset_ss$GRADE = factor("1",c("5","4","3","2","1"))
dataset_ss[dataset_ss$pre >= 0   & dataset_ss$pre < 750,]$GRADE = "5"
dataset_ss[dataset_ss$pre >= 750 & dataset_ss$pre < 900,]$GRADE = "4"
dataset_ss[dataset_ss$pre >= 900 & dataset_ss$pre < 970,]$GRADE = "3"
dataset_ss[dataset_ss$pre >= 971 & dataset_ss$pre < 1000,]$GRADE = "2"



ggplot(dataset_ss, aes(x=GRADE)) +
  geom_bar(position ="dodge", width=0.8, colour="black", stat ="identity") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  ggtitle("Bar chart") 
  theme(plot.title=element_text(face="bold", hjust=0.5,size=15,color="black")) +
  labs(y="COUNT") 
  

  
ggplot(dataset_ss, aes(x=GRADE)) +
  geom_bar(position ="dodge", width=0.8, colour="black") +
  theme_bw() +
  ggtitle("Bar chart") +
  theme(plot.title=element_text(face="bold", hjust=0.5,size=15,color="black")) +
  labs(y="COUNT")

  head(dataset_ss,10)
str(dataset_ss)

