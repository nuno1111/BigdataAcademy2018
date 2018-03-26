#============================================================
# title: "고급회귀분석"
# subtitle: "Advanced Regression Analysis"
# author: "Begas"
# date: "2018"
#============================================================
#그림 크기 옵션
options(repr.plot.width = 6, repr.plot.height = 5) 

#============================================================
# 주성분 회귀분석
#============================================================

#============================================================
# 데이터 설명
#============================================================

# 프랑스 경제의 수입활동에 관한 총계 데이터
# 사용 변수 : Import(총수입), Doprod(국내 총생산), Stock(주식 형성), consum(국내 총소비) 
# 모든 변수는 1949~1966년까지 10억 단위의 프랑스 프랑(franc) 단위로 측정
import_df <- read.csv("3월23일/import.csv")
import_df <- subset(import_df, select = -c(year))
head(import_df)
# import: 총수입
# doprod: 국내 총생산
# stock: 주식 형성
# consum: 국내 총소비

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits = 2)
  txt <- paste0("   R = ", r)
  text(0.5, 0.3, txt, cex = 1)
}

# Customize upper panel
upper.panel<-function(x, y){
  points(x, y, pch = 19, col = "#FC4E07")
}

pairs(import_df, lower.panel = panel.cor, upper.panel = upper.panel)

# 주성분 분석
import_pca <- princomp(import_df[-1], cor = TRUE)
summary(import_pca)
summary(import_pca)$loadings

# scree plot
#install.packages("factoextra")
#install.packages("ggplot2")
library("factoextra")
library("ggplot2")
fviz_eig(import_pca, main = "Scree Plot", barcolor = "#00AFBB", barfill = "#00AFBB", 
         linecolor = "#FC4E07", xlab = "Principal Component", ylab = "Proportion of Variance")

# score값  
import_score <- data.frame(summary(import_pca)$score) 
# 제3주성분까지 모두 이용한 회귀모형 
import_score_lm <- lm(import_df$import  ~ Comp.1 + Comp.2 + Comp.3, data = import_score)
summary(import_score_lm)
# 제3주성분 유의하지 않음

# 제2주성분까지 모두 이용한 회귀모형 
import_score_lm <- lm(import_df$import ~ Comp.1 + Comp.2, data = import_score)
summary(import_score_lm)
# 모두 유의함

#============================================================
# 부분 최소 제곱
#============================================================
#install.packages("pls")
library(pls)

#============================================================
# 데이터 설명
#============================================================
# 옥탄의 수(octane)와 401개의 값으로 이루어진 NIR Spectrum을 가지는 자료
# 관측치의 개수 : 60개
# 관측치개수 < 변수개수

data(gasoline)

# 모형적합
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasoline, validation = "CV")
summary(gas1)

plot(RMSEP(gas1), legendpos = "topright")

# 성분의 개수가 2개까지 급격히 감소하므로 최적의 성분 개수는 2개
# 최적의 성분 개수인 2개의 성분을 가지는 부분 최소 제곱 회귀 모형 적합
gas2 <- plsr(octane ~ NIR, ncomp = 2, data = gasoline)
plot(gas1$fitted.values[, , 2], gasoline$octane, pch = 19,
     col = "dodgerblue", xlab = "fitted value", ylab = "real")
abline(0, 1, col = 2, lwd = 1.5)
# 적합치와 실제갑이 매우 유사

#============================================================
# Shrinkage Method
#============================================================
#install.packages("glmnet")
library(glmnet)
# 데이터 설명
# 대형 금융 기관의 30명의 사무 직원의 설문조사 자료 
# 각 변수의 숫자는 각 부분에서 호의적인 평가의 비율 
# 반응변수: rating
# 설명변수: complaints, privileges, learning, raises, critical, advance 
data(attitude)
head(attitude)

pairs(attitude, lower.panel = panel.cor, upper.panel = upper.panel)

# 반응 변수 지정
y <- attitude[, 1]
# 설명 변수 지정
x <- attitude[, -1]

#============================================================
# Ridge 
#============================================================
ridge.fit <- glmnet(as.matrix(x), y, alpha = 0) # alpha=0: ridge, 1: lasso
# 최적 조율 모수
set.seed(1234) # random number fix
cv.ridge <- cv.glmnet(as.matrix(x), y, alpha = 0)
plot(cv.ridge)
round(cv.ridge$lambda.min, 4)
# shrinkage factor가 1.4332일 때 MSE가 가장 작으므로 best model

# Ridge 회귀계수
coef(ridge.fit, s = cv.ridge$lambda.min)

# 모든 변수의 계수가 추정됨
#install.packages("plotmo")
library(plotmo)
plot_glmnet(ridge.fit, label = TRUE)

#============================================================
# Lasso
#============================================================
lasso.fit <- glmnet(as.matrix(x), y, alpha = 1)
# 최적 조율 모수
set.seed(1234) 
cv.lasso <- cv.glmnet(as.matrix(x), y, alpha = 1)  
plot(cv.lasso)
round(cv.lasso$lambda.min, 4)
# shrinkage factor가 1.1625일 때 MSE가 가장 작으므로 best model

# Lasso 회귀계수
coef(lasso.fit, s = cv.lasso$lambda.min)
# complaints, learning만 선택되어 모형적합
plot_glmnet(lasso.fit, label = TRUE)

#============================================================
# Elasticnet
#============================================================
elas.fit <- glmnet(as.matrix(x), y, alpha = 0.5)
# 최적 조율 모수
set.seed(1234)
cv.elas <- cv.glmnet(as.matrix(x), y, alpha = 0.5) 
plot(cv.elas)
round(cv.elas$lambda.min, 4)
# - shrinkage factor가 1.6026일 때 MSE가 가장 작으므로 best model

# Elasticnet 회귀계수
coef(elas.fit, s = cv.elas$lambda.min)
# complaints, learning, raises, advance 변수 선택됨
plot_glmnet(elas.fit, label = TRUE)

