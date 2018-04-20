#### Classfication ####
# DataSet Loading
dataset_poi <- read.csv("./DATA/DATASET_POI.csv")
head(dataset_poi)

# 임시 sampling 10%
set.seed(1234)  # seed 고정
idx <- sample(1:nrow(dataset_poi), nrow(dataset_poi) * 0.1)
dataset_poi <- dataset_poi[idx, ]

#### MAKE_DATESET ####

# 명목형 y값 만들기
qt <- quantile(dataset_poi$VALUE, c(.125,.250,.375,.5,.625,.750,.875))
qt <- c(-Inf,qt,Inf)
dataset_poi$LABEL <- cut(dataset_poi$VALUE, breaks = qt, labels = c(1:8)) # VALUE를 명목형 변수로 변경

dataset_poi <- dataset_poi[,c(-1,-2,-7,-8,-9)]
dataset_poi$YYYYMM <- as.factor(dataset_poi$YYYYMM)
dataset_poi$HOUR <- as.factor(dataset_poi$HOUR)

#### 데이터 분할 ####
idx <- sample(1:nrow(dataset_poi), nrow(dataset_poi) * 0.7)  # training set index 설정
train_df <- dataset_poi[idx, ]  # training set 분할
test_df <- dataset_poi[-idx, ]  # test set 분할

#### SVM ####
# install.packages("e1071")
# library(e1071)  # library 불러오기
# fit.svm.linear <- svm(LABEL~., data=train_df, kernel = "linear")  # svm 모형 생성
# fit.svm.linear  # 모형 결과 출력

##### Classification Plot ####
# plot(fit.svm.linear, train_df, LPR ~ PEG)

# Decision Tree : 느리네요

#### Decision Tree ####
# install.packages("tree")
# library(tree)
# treemod <- tree(LABEL~. , data=train_df, split="deviance")
# plot(treemod)
# text(treemod)

# cv.trees<-cv.tree(treemod, FUN=prune.misclass ) # for classification decision tree
# plot(cv.trees)

# Decision Tree : 버리자...

#### Decision Tree(rpart) ####
# install.packages("tree")
library(rpart)
(treemod <- rpart(LABEL~. , data=train_df))
plot(treemod)
text(treemod)

# cv.trees<-cv.tree(treemod, FUN=prune.misclass ) # for classification decision tree
# plot(cv.trees)

# Decision Tree : 버리자...


#### RandomForest Tree ####
# install.packages("randomForest")
library(randomForest)
fit.rf <- randomForest(LABEL~.,data=train_df, ntree=20, mtry=5)   #변수선택 k를 3로 설정

#예측
pred <- predict(fit.rf,test_df)
table(pred, test_df$LABEL)

#오분류율
mean(pred != test_df$LABEL)
# RandomForest Tree Accuracy: 38% 

#random Forest에서 k정하기
MAPE <- NULL
for(i in 1:ncol(train_df)){
  cat(i)
  temp_rf <- randomForest(LABEL~.,data=train_df, ntree=20, mtry=i)
  pred <- predict(temp_rf,test_df)
  MAPE[i] <- mean(pred!=test_df$LABEL)
}
round(MAPE,2)
plot(x=seq(1,ncol(train_df)),y=MAPE,type="l",xlab="변수개수")


