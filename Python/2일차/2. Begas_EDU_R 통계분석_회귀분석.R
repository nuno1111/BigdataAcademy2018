
# Python 통계분석 ----

# 1. t-test ----

set.seed(1)

# data1 : 평균 0, 표준편차 1, size 50
# data2 : 평균 1, 표준편차 1, size 50
data1 <- rnorm(n = 50, mean = 0, sd = 1)
data2 <- rnorm(n = 50, mean = 1, sd = 1)

# 평균 표준편차 출력
mean(data1); sd(data1)
mean(data2); sd(data2)


# One Sample t-test
# 모집단의 평균이 0인지 검정
true_mu <- 0
t.test(data1, mu = true_mu)
t.test(data2, mu = true_mu)

# Two Sample t-test
t.test(x = data1, y = data2, var.equal = TRUE)

# 히스토그램으로 분포 확인
hist(x = data1, col = rgb(1, 0, 0, 0.5), xlim = c(-4, 4), ylim = c(0, 15), 
     main = 'Overlapping Histogram', xlab = 'Variable')
hist(x = data2, col = rgb(0, 0, 1, 0.5), add = T)
box()




# 2. 상관분석 ----

plot(x = data1, y = data2, col = 'blue', pch = 16)

# 상관계수
cor(x = data1, y = data2)

# 상관관계 검정
cor.test(x = data1, y = data2)

# matrix 1개로 모든 컬럼 조합별 상관계수 산출
cor(cbind(a = rnorm(10, 1, 1), b = 1:10, c = 2:11))




# 3. 선형 회귀분석 ----

install.packages('lars')
require(lars)

# 데이터 로드
data(diabetes)

# 데이터셋 구성 확인
str(diabetes)

# diabetes$x 차원 확인
dim(diabetes$x)

# diabetes$y 차원 확인
length(diabetes$y)

# AsIs 클래스를 데이터 프레임으로 변환
unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  return(X)
}

x_df <- as.data.frame(unAsIs(diabetes$x))
y    <- diabetes$y

# 데이터 처리
diabetes_X <- x_df$bmi
length(diabetes_X)

# 설명변수
X_train <- diabetes_X[1:(length(diabetes_X)-20)]
X_test  <- diabetes_X[(length(diabetes_X)-20+1):length(diabetes_X)]

# 종속변수
y_train <- diabetes$y[1:(length(diabetes$y)-20)]
y_test  <- diabetes$y[(length(diabetes$y)-20+1):length(diabetes$y)]

regr <- lm(y_train ~ X_train)

summary(regr)


# model fitting 결과
# SStotal = SSredisual + SSregression
fitted_val <- regr$fitted.values
SST  <- sum((y_train - mean(y_train))^2)
SSE  <- sum((y_train - fitted_val)^2)
SSR  <- sum((fitted_val - mean(y_train))^2)

print(paste0('SST : ', SST))
print(paste0('SSE + SSR : ', SSE + SSR))
print(paste0('r.square : ', SSR / SST))

# model prediction 결과
pred_val <- predict(regr, newdata = data.frame(X_train = X_test))

SST <- sum((y_test - mean(y_test))^2)
SSE <- sum((y_test - pred_val)^2)
SSR <- sum((pred_val - mean(y_test))^2)

MSE   <- SSE / length(pred_val)
score <- 1 - (SSE/SST)

print(paste0('MSE : ', MSE))
print(paste0('score : ', score))

# 결과 plot
plot(x = X_test, y = y_test, pch = 16, col = "red")
abline(regr, col = rgb(0, 0, 1, 1), lwd = 3)
