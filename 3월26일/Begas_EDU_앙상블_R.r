#============================================================
# title: "앙상블"
# subtitle: "Ensemble"
# author: "Begas"
# date: "2018"
#============================================================

#install.packages('ipred')
#install.packages('gbm')
#install.packages("randomForest")

#예제데이터
en_data <- read.csv("tree.csv")
head(en_data)

#train/ test 나누기
set.seed(123)
sample.num  <- sample(1:nrow(en_data), 0.7*nrow(en_data))
train       <- en_data[sample.num,]
test        <- en_data[-sample.num,]
x.train <- train[,-6]
x.test <- test[,-6]
y.train <- train[,6]
y.test <- test[,6]

#Bagging
library(ipred)
fit.bagg <- ipredbagg(as.factor(y.train), x.train, data=train, nbagg=1000)  
#bootstrap의 개수는 충분히 크게 하는것이 좋다.
fit.bagg

#예측
pred<-predict(fit.bagg,x.test)
table(pred,y.test)

#오분류율
mean(pred!=y.test)

#확률예측
pred2<-predict(fit.bagg,x.test,type="prob")
head(pred2)


#Boosting
library(gbm)
fit.boost <- gbm(as.factor(UNS)~.,data=train,distribution="multinomial",n.trees=1000)
fit.boost

#예측
pred.prob<-predict(fit.boost,x.test,type="response",n.trees=1000)  #확률로 나옴
pred.prob=matrix(pred.prob,ncol=4)                                 #4가지 범주의 확률

#확률예측
colnames(pred.prob) <- levels(y.train)
head(pred.prob)

#4가지 범주중에 가장 높은 확률을 가진 범주로 분류
pred<-apply(pred.prob,1,which.max)
pred<-ifelse(pred==1,"High",ifelse(pred==2,"Low",ifelse(pred==3,"Middle","very_low")))
table(pred,y.test)

#오분류율
mean(pred!=y.test)

#Boosting 횟수 M 정하기
find_M <- function(m){
    fit.boost <- gbm(as.factor(UNS)~.,data=train,distribution="multinomial",n.trees=m)
    pred.prob<-predict(fit.boost,x.test,type="response",n.trees=m)  #확률로 나옴
    pred.prob=matrix(pred.prob,ncol=4)
    pred<-apply(pred.prob,1,which.max)
    pred<-ifelse(pred==1,"High",ifelse(pred==2,"Low",ifelse(pred==3,"Middle","very_low")))
    print(mean(pred!=y.test))
}

find_M(50)
find_M(100)
find_M(500)
find_M(1000)
find_M(5000)
find_M(10000)


#Random Forest
library(randomForest)
fit.rf <- randomForest(as.factor(UNS)~.,data=train, ntree=1000, mtry=3)   #변수선택 k를 3로 설정

#예측
pred<-predict(fit.rf,x.test)
table(pred,y.test)

#오분류율
mean(pred!=y.test)

#random Forest에서 k정하기
MAPE <- NULL
for(i in 1:ncol(x.train)){
    temp_rf <- randomForest(as.factor(UNS)~.,data=train, ntree=1000, mtry=i)  
    pred <- predict(temp_rf,x.test)
    MAPE[i] <- mean(pred!=y.test)
}
round(MAPE,2)
plot(x=seq(1,ncol(x.train)),y=MAPE,type="l",xlab="변수개수")



