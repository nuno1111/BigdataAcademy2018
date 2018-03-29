#============================================================
# title: "의사결정나무"
# subtitle: "Decision Tree"
# author: "Begas"
# date: "2018"
#============================================================

#install.packages("tree")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("C50")
#install.packages("party")

#예제데이터
tree_data <- read.csv("3월26일/tree.csv")
head(tree_data)

#Training set 70% , Test set 30%
set.seed(123)
sample.num  <- sample(1:nrow(tree_data), 0.7*nrow(tree_data))
train       <- tree_data[sample.num,]
test        <- tree_data[-sample.num,]
dim(train)
dim(test)

#tree
library(tree)
tree_ml<-tree(UNS~., data=train, split="deviance")
# tree_ml<-tree(UNS~., data=train, split="gini")
tree_ml
plot(tree_ml)
text(tree_ml)
summary(tree_ml)

#tree 가지치기 (노드수로)
tree_p <- snip.tree(tree_ml, nodes=c(10))
plot(tree_p)
text(tree_p, all=T)

#tree 가지치기 (끝마디수로)
tree_p2 <- prune.misclass(tree_ml)
plot(tree_p2)
fin.tr  <- prune.misclass(tree_ml, best=6)
plot(fin.tr)
text(fin.tr)

#Tree 가지치기 비교
par(mfrow=c(2,2))
plot(tree_p2)
plot(tree_ml, main="raw tree"); text(tree_ml,cex=0.7) #가지치기 안한 Tree
plot(tree_p, main="prune by number of nodes"); text(tree_p, all=T, cex=0.7)  #node수로 가지치기한 Tree
plot(fin.tr, main="prune by number of Terminal nodes"); text(fin.tr, cex=0.7)  #끝마디수로 가지치기한 Tree


#tree 예측
yhat  <- predict(fin.tr, newdata=test, type="class")
ytest <- test[,6]
table(yhat,ytest)
cat("오분류율 = ",sum(yhat!=ytest)/length(yhat),"%")


#공간분할
par(pty="s")
plot(train[,4], train[,5], type="n", xlab="LPR", ylab="PEG")
text(train[,4], train[,5],col="dodgerblue",c("H","L","M","VL")[train[,6]])
partition.tree(fin.tr, add=TRUE, cex=1.5)



#CART
library(rpart)
library(rpart.plot)

cart_ml <- rpart(UNS ~.,train)
cart_ml
plot(cart_ml)
text(cart_ml, all=T)

#CART - 시각적인 그래프
prp(cart_ml, type=4, extra=1, digits=3)

#CART - 예측
yhat  <- predict(cart_ml, newdata=test, type="class")
ytest <- test[,6]
table(yhat,ytest)
cat("오분류율 = ",sum(yhat!=ytest)/length(yhat),"%")


#C5.0
library(C50)

c5_ml  <- C5.0(UNS ~.,train)
summary(c5_ml)
plot(c5_ml)

#C5.0 - 예측
yhat  <- predict(c5_ml, newdata=test, type="class")
ytest <- test[,6]
table(yhat,ytest)
cat("오분류율 = ",sum(yhat!=ytest)/length(yhat),"%")


#QUESET
library(party)

queset_ml <- ctree(UNS ~., train, controls = ctree_control(testtype=c("Bonferroni")))
summary(queset_ml)
plot(queset_ml)

#QUESET - 예측
yhat  <- predict(queset_ml, newdata=test, type="response")
ytest <- test[,6]
table(yhat,ytest)
cat("오분류율 = ",sum(yhat!=ytest) / length(yhat), "%")


