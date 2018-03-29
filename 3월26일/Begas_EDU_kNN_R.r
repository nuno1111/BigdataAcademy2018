#============================================================
# title: "k-Nearest Neighbors"
# subtitle: "k-NN"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# k-NN
# (Q) 신용승인이 날것인지 분류 할수 있을까?
# 데이터 : Credit Approval(2013)
#============================================================

# 데이터 입력
credit_df <- read.csv("3월26일/credit.csv")
head(credit_df)

# 데이터 분할을 위한 seed 고정
set.seed(1234)
# training set index 설정
idx <- sample(1:653, 653*0.8)
# x data training set 분할
tr_x_df <- credit_df[idx, -16] 
# y data train set 분할
tr_y_df <- factor(credit_df[idx, 16])
# x data test set 분할
ts_x_df <- credit_df[-idx, -16]
# y data test set 분할
ts_y_df <- credit_df[-idx, 16] 

# library 불러오기
library(class)
# k에 대한 1 부터 21까지 홀수 grid 생성
k.grid <- seq(1 ,21, 2) 
# 모형 정확도 저장 공간 생성
acc <- c()
# k에 따른 모형 정확도 계산 loop 
for(i in k.grid){ # loop start
  # k에 따른 모형 적합
  knn_model <- knn.cv(tr_x_df, k = i, cl = tr_y_df)
  # 각 k에 대한 정확도 계산
  error <- sum(knn_model == tr_y_df) / 522
  # 전체 k에 대한정확도 저장
  acc <- c(acc, error)
} # loop end
# k에 대한 모형 정확도 plot
plot(k.grid, acc, type = "b")
# 가장 높은 모형 정확도를 가진 k 확인
optimal.k <- k.grid[which.max(acc)]
print(optimal.k)
# optimal k를 통한 모형 Test
knn_model <- knn(train = tr_x_df, test = ts_x_df, cl = tr_y_df, k = optimal.k)
# Test Confusion Matrix
table(knn_model, ts_y_df)
# Test 정확도 계산
sum(diag(table(knn_model, ts_y_df))) / length(ts_y_df)

