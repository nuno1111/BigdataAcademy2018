
# Python 통계분석 ----

# 1. 로지스틱 회귀분석 ----
file_path <- paste(Sys.getenv("USERPROFILE"), "Desktop/kdata/1. data/titanic.csv", sep = "/")

data_df <- read.csv(file_path)
str(data_df)
head(data_df)

# 더미 변수
data_df$Sex_male <- ifelse(data_df$Sex =='male', 1, 0)
data_df$Pclass_1 <- ifelse(data_df$Pclass == 1, 1, 0)
data_df$Pclass_2 <- ifelse(data_df$Pclass == 2, 1, 0)
data_df$Pclass_3 <- ifelse(data_df$Pclass == 3, 1, 0)

# 분석 데이터
anal_data_df <- subset(data_df, select = c("Survived", "Age", "Fare", "Pclass_1", "Pclass_2", "Pclass_3", "Sex_male"))
head(anal_data_df)


train_index <- sample(1:nrow(anal_data_df), 624, replace = FALSE)
test_index  <- setdiff(1:nrow(anal_data_df), train_index)

length(train_index)
length(test_index)

anal_data_df[train_index, ]
anal_data_df[test_index, ]

# train/test 데이터 나누기
train_df <- anal_data_df[c(1 : 841), ]
test_df  <- anal_data_df[c(842 : 891), ]

dim(train_df)
dim(test_df)

# model fitting
logit_fit <- glm(Survived ~ Age + Fare + as.factor(Pclass) + Sex_male, data = train_df, family = 'binomial')

# predict test set
test_df$predict <- predict(logit_fit, newdata = test_df, type = 'response')

cutoff_value <- 0.5
test_df$pred_val <- ifelse(test_df$predict >= cutoff_value, 1, 0)

table(test_df$Survived, test_df$pred_val)

install.packages("splitstackshape")
library(splitstackshape)


head(data_df)

out <- stratified(data_df, "Pclass", .6, bothSets = FALSE)

?stratified

names(out)

head(out)

table(out$Pclass)
table(data_df$Pclass)

# 2. 군집분석 ----
head(iris)
str(iris)
names(iris)


#표준화
anal_data_df <- scale(iris[, -5])
summary(iris)
summary(anal_data_df)

iris3_fit <- kmeans(anal_data_df[, c("Petal.Length", "Petal.Width")], 3)
iris5_fit <- kmeans(anal_data_df[, c("Petal.Length", "Petal.Width")], 5)
iris8_fit <- kmeans(anal_data_df[, c("Petal.Length", "Petal.Width")], 8)

table(iris$Species, iris3_fit$cluster)
table(iris$Species, iris5_fit$cluster)
table(iris$Species, iris8_fit$cluster)

# 최적 군집개수 선택
install.packages("NbClust")
library(NbClust)
nc <- NbClust(anal_data_df, min.nc = 2, max.nc = 10, method = "kmeans")
par(mfrow = c(1, 1))
barplot(table(nc$Best.nc[1,]), xlab = "Number of Clusters", main = "Choosing the number of clusters")



# 군집분석 iris ----

install.packages("caret")
library(caret)

set.seed(123)

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)

head(iris[inTrain,])

table(iris[inTrain, "Species"])


train_df <- iris[inTrain,]
test_df  <- iris[-inTrain,]
dim(train_df)
dim(test_df)

# scale
anal_data_df <- scale(train_df[-5])
summary(anal_data_df)


# k-means clustering
iris3_fit <- kmeans(anal_data_df, 3)

names(iris3_fit)

train_df$cluster <- as.factor(iris3_fit$cluster)

qplot(Petal.Width, Petal.Length, colour = cluster, data = train_df)

table(train_df$Species, iris3_fit$cluster)

anal_data_df <- as.data.frame(anal_data_df)



# 분류분석 iris ----
install.packages("e1071")
library(e1071)

# Classification Modeing & Prediction by rpart(Tree model)
# model fitting
rpart_fit <- train(x = anal_data_df, y = train_df$Species, method = "rpart")

# scaling
anal_test_df <- test_df
for(i in 1:4)
{
  anal_test_df[, i] = (anal_test_df[, i] - mean(train_df[, i])) / sd(train_df[, i])
}

anal_test_df <- as.data.frame(anal_test_df)

head(anal_test_df)

# prediction
predict_val  <- predict(rpart_fit, anal_test_df)

# result
table(predict_val, test_df$Species)
