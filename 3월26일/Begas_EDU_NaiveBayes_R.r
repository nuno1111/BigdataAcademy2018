
library(e1071)

iris <- read.csv('Iris_1.csv')
head(iris)

# training data: 70% / test data : 30%
set.seed(1004)
tr.index <- sample(1:nrow(iris), nrow(iris) * 0.7)
te.index <- setdiff(1:nrow(iris), tr.index)

model <- naiveBayes(Species ~ ., data = iris[tr.index, ])

pred <- predict(model, iris[te.index, ], type = "raw")
head(pred)

pred_1 <- predict(model, iris[te.index, ])
head(pred_1)

table(pred_1, iris$Species[te.index])

exercise <- read.csv('exercise.csv')
head(exercise)

model_1 <- naiveBayes(운동여부 ~ ., data = exercise)

test_df <- data.frame(날씨 = "맑음", 온도 = "시원함", 습도 = "높음", 바람 = "많이")
test_df

pred <- predict(model_1, test_df, type = "raw")
head(pred_1)

pred_1 <- predict(model_1, test_df)
pred_1
