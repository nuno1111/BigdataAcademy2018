#============================================================
# title: "�ӻ��"
# subtitle: "Ensemble"
# author: "Begas"
# date: "2018"
#============================================================

#install.packages('ipred')
#install.packages('gbm')
#install.packages("randomForest")

#����������
en_data <- read.csv("tree.csv")
head(en_data)

#train/ test ������
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
#bootstrap�� ������ ����� ũ�� �ϴ°��� ����.
fit.bagg

#����
pred<-predict(fit.bagg,x.test)
table(pred,y.test)

#���з���
mean(pred!=y.test)

#Ȯ������
pred2<-predict(fit.bagg,x.test,type="prob")
head(pred2)


#Boosting
library(gbm)
fit.boost <- gbm(as.factor(UNS)~.,data=train,distribution="multinomial",n.trees=1000)
fit.boost

#����
pred.prob<-predict(fit.boost,x.test,type="response",n.trees=1000)  #Ȯ���� ����
pred.prob=matrix(pred.prob,ncol=4)                                 #4���� ������ Ȯ��

#Ȯ������
colnames(pred.prob) <- levels(y.train)
head(pred.prob)

#4���� �����߿� ���� ���� Ȯ���� ���� ���ַ� �з�
pred<-apply(pred.prob,1,which.max)
pred<-ifelse(pred==1,"High",ifelse(pred==2,"Low",ifelse(pred==3,"Middle","very_low")))
table(pred,y.test)

#���з���
mean(pred!=y.test)

#Boosting Ƚ�� M ���ϱ�
find_M <- function(m){
    fit.boost <- gbm(as.factor(UNS)~.,data=train,distribution="multinomial",n.trees=m)
    pred.prob<-predict(fit.boost,x.test,type="response",n.trees=m)  #Ȯ���� ����
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
fit.rf <- randomForest(as.factor(UNS)~.,data=train, ntree=1000, mtry=3)   #�������� k�� 3�� ����

#����
pred<-predict(fit.rf,x.test)
table(pred,y.test)

#���з���
mean(pred!=y.test)

#random Forest���� k���ϱ�
MAPE <- NULL
for(i in 1:ncol(x.train)){
    temp_rf <- randomForest(as.factor(UNS)~.,data=train, ntree=1000, mtry=i)  
    pred <- predict(temp_rf,x.test)
    MAPE[i] <- mean(pred!=y.test)
}
round(MAPE,2)
plot(x=seq(1,ncol(x.train)),y=MAPE,type="l",xlab="��������")



