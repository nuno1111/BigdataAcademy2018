#============================================================
# title: "Support Vector Machine"
# subtitle: "SVM"
# author: "Begas"
# date: "2018"
#============================================================

###### 데이터 불러 오기
ukm_df <- read.csv("3월26일/ukm.csv")  # csv 파일 읽기
head(ukm_df, 5)   # Data의 첫 5번째 줄 까지 보기

###### 데이터 분할
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(ukm_df), nrow(ukm_df) * 0.7)  # training set index 설정
train_df <- ukm_df[idx, ]  # training set 분할
test_df <- ukm_df[-idx, ]  # test set 분할

###### 기본적인 분석
# install.packages("e1071")
library(e1071)  # library 불러오기
fit.svm.linear <- svm(UNS~., data=train_df, kernel = "linear")  # svm 모형 생성
fit.svm.linear  # 모형 결과 출력

##### Classification Plot
plot(fit.svm.linear, train_df, LPR ~ PEG)

#============================================================
# 선형 SVM
# (Q) Electrical DC 기계 연구에 대한 사용자 지식 수준을 잘 분류할 수 있을까?
# 데이터 : User Knowledge Modeling Data Set(2013)
# * STG : 목표 객체에 대한 학습 시간
# * SCG : 목표 객체에 대한 반복 횟수
# * STR : 목표 객체와 관련있는 객체에 대한 사용자 학습 시간
# * LPR : 목표 객체와 관련있는 객체에 대한 사용자의 시험 성능
# * PEG : 목표 객체에 대한 사용자의 시험 성능
# * UNS : 사용자의 지식 수준
#============================================================

### 모형 Tuning
obj.linear <- tune.svm(UNS~., data=train_df, kernel="linear", gamma=2^(-7:7), cost=2^(-7:7))  # 선형 svm tuning
obj.linear$best.model  # tuning 한 best model 출력

### 모형 Test
pred.linear<-predict(obj.linear$best.model, test_df)

### 모형 정확도 확인
table(pred.linear, test_df$UNS)  # confusion matirx 출력
acc <- sum(diag(table(pred.linear, test_df$UNS)))/ sum(table(pred.linear, test_df$UNS)) # 정확도 계산
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")  # 정확도 출력
# 선형 SVM을 통한 분류 정확도 91.03%로 나타남

### 선형 SVM Classfication Plot
plot(obj.linear$best.model, test_df, LPR ~ PEG)

#============================================================
# 비선형 SVM Radial Kernel
# (Q) Electrical DC 기계 연구에 대한 사용자 지식 수준을 잘 분류할 수 있을까?
# 데이터 : User Knowledge Modeling Data Set(2013)
# * STG : 목표 객체에 대한 학습 시간
# * SCG : 목표 객체에 대한 반복 횟수
# * STR : 목표 객체와 관련있는 객체에 대한 사용자 학습 시간
# * LPR : 목표 객체와 관련있는 객체에 대한 사용자의 시험 성능
# * PEG : 목표 객체에 대한 사용자의 시험 성능
# * UNS : 사용자의 지식 수준
#============================================================

### 모형 Tuninig
obj.radial <- tune.svm(UNS~., data=train_df, kernel="radial", gamma=2^(-7:7), cost=2^(-7:7))  # Radial kernel을 통한 비선형 svm tuning
obj.radial$best.model  # tuning 한 best model 출력

### 모형 Test
pred.radial <- predict(obj.radial$best.model, test_df)

### 모형 정확도 확인
table(pred.radial, test_df$UNS)  # confusion matrix 출력
acc <- sum(diag(table(pred.radial, test_df$UNS)))/ sum(table(pred.radial, test_df$UNS))  # 정확도 계산
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")  # 정확도 출력
# Radial 커널을 활용한 비선형 SVM을 통한 분류 정확도는 89.74%로 나타남 

### 비선형 SVM Radial kernel Classfication Plot
plot(obj.radial$best.model, test_df, LPR ~ PEG)

#============================================================
# 비선형 SVM Sigmoid Kernel
# (Q) Electrical DC 기계 연구에 대한 사용자 지식 수준을 잘 분류할 수 있을까?
# 데이터 : User Knowledge Modeling Data Set(2013)
# * STG : 목표 객체에 대한 학습 시간
# * SCG : 목표 객체에 대한 반복 횟수
# * STR : 목표 객체와 관련있는 객체에 대한 사용자 학습 시간
# * LPR : 목표 객체와 관련있는 객체에 대한 사용자의 시험 성능
# * PEG : 목표 객체에 대한 사용자의 시험 성능
# * UNS : 사용자의 지식 수준
#============================================================

## 비선형 SVM Sigmoid Kernel
### 모형 Tuninig
obj.sigmoid <- tune.svm(UNS~., data=train_df, kernel="sigmoid", gamma=2^(-7:7), cost=2^(-7:7))  # Sigmoid kernel을 통한 비선형 svm tuning
obj.sigmoid$best.model  # tuning한 best model 출력

### 모형 Test
pred.sigmoid <- predict(obj.sigmoid$best.model, test_df)

### 모형 정확도 확인
table(pred.sigmoid, test_df$UNS)  # confusion matrix 출력 
acc <- sum(diag(table(pred.sigmoid, test_df$UNS)))/ sum(table(pred.sigmoid, test_df$UNS))  # 정확도 계산
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")  # 정확도 출력
# Sigmoid 커널을 활용한 비선형 SVM을 통한 분류 정확도는 92.31%로 나타남

### 비선형 SVM Sigmoid kernel Classfication Plot
plot(obj.sigmoid$best.model, test_df, LPR ~ PEG)

#============================================================
# 비선형 SVM Polynomial kernel
# (Q) Electrical DC 기계 연구에 대한 사용자 지식 수준을 잘 분류할 수 있을까?
# 데이터 : User Knowledge Modeling Data Set(2013)
# * STG : 목표 객체에 대한 학습 시간
# * SCG : 목표 객체에 대한 반복 횟수
# * STR : 목표 객체와 관련있는 객체에 대한 사용자 학습 시간
# * LPR : 목표 객체와 관련있는 객체에 대한 사용자의 시험 성능
# * PEG : 목표 객체에 대한 사용자의 시험 성능
# * UNS : 사용자의 지식 수준
#============================================================

### 모형 Tuninig
# Polynomial kernel을 통한 비선형 svm tuning
obj.polynomial <- tune.svm(UNS~., data=train_df, kernel="polynomial", gamma=2^(-7:7), cost=2^(-7:7))   
obj.polynomial$best.model  # tuning한 best model 출력

### 모형 Test
pred.polynomial <- predict(obj.polynomial$best.model, test_df)

### 모형 정확도 확인
table(pred.polynomial, test_df$UNS)  # confusion matrix 출력
acc <- sum(diag(table(pred.polynomial, test_df$UNS)))/ sum(table(pred.polynomial, test_df$UNS))  # 정확도 계산
cat("모형 정확도 : ", round(acc*100, 2), "%", "\n")  # 정확도 출력
# Polynomial 커널을 활용한 비선형 SVM을 통한 분류 정확도ㄴ 82.05%로 나타남

### Classfication Plot
plot(obj.polynomial$best.model, test_df, LPR ~ PEG)
