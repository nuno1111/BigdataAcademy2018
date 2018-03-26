#============================================================
#title: "선형회귀분석"
#subtitle: "Linear Regression"
#author: "Begas"
#date: "2018"
#============================================================

## 예제 데이터
#(Q) tv, 라디오 광고비는 매출액에 어떠한 영향을 끼칠까?

reg_data<-read.csv("3월21일/Linear.csv")
#install.packages("tidyverse")
#install.packages("car")
#install.packages("tseries")
#install.packages("lmtest")
head(reg_data)

#- y : 매출액
#- x1 : tv 광고비
#- x2 : 라디오 광고비


# 데이터 살펴보기 - **산점도**
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  cex.cor <- 5
  text(0.5, 0.5, txt, cex = r)
}
pairs(reg_data, diag.panel = panel.hist, lower.panel = panel.cor)

# 데이터 살펴보기 - **Boxplot**
library(tidyverse)
ggplot(reg_data,aes(x="",y=y))+geom_boxplot(fill="gray80")+
  theme_classic()

#===========================================================
#                      단순 선형회귀분석
#===========================================================
#회귀모형 생성
reg_model <- lm(y~x1, data=reg_data)
reg_model

#산점도와 적합된 회귀직선
plot(reg_data$x1,reg_data$y, xlab='TV 광고비', ylab='매출액', main='산점도')
abline(reg_model, col=2) 
abline(a=0, b=1, col=4) 

#회귀모형에 대한 정보들
summary(reg_model)

## regression lab
x <- 1:50
set.seed(100)
e <- rnorm(50, 0, 0.3)
y <- log(x) + e

cor(x,y)
cor(log(x), y)

fit1 <- lm(y~x)
fit2 <- lm(y~log(x))
summary(fit1)
summary(fit2)

par(mfrow = c(1,2))
plot(x,y)
abline(fit1, col = 2)
plot(log(x),y)
abline(fit2, col = 2)

#===========================================================
#                      잔차분석
#===========================================================
#단순 선형회귀분석 예 
reg_data$company <- c(1:12)

R=lm(y~x1,data=reg_data)
summary(R)
plot(reg_data$y~reg_data$x1,pch=19, xlab='TV 광고비',ylab='매출액')
abline(R,col="red")
par(mfrow=c(2,2))
plot(R)

cbind(sales=reg_data$y, predict(R, interval="confidence"), residual=summary(R)$res)


#===========================================================
#                      선형회귀분석
#===========================================================
# 선형 회귀모형
reg_model <- lm(y~x1+x2, data=reg_data)
reg_model
#- y = 1.092 + 1.563 x1 + 0.064 x2
#- tv광고비를 1단위 증가하면 매출액이 1.563만큼 증가
#- 라디오 광고비를 1단위 증가하면 매출액이 0.064만큼 증가

summary(reg_model)

# 예측
pre1<- predict(reg_model)
reg_data$pre<-pre1
ggplot(reg_data,aes(y,pre))+geom_point(color="gray20",size=2)+
  geom_abline(color="red",size=1.2)+theme_classic()+
  labs(x="Observed",y="Predicted")

# 잔차분석 - 독립성 (Durbin-Watson 통계량)
library(car)
durbinWatsonTest(reg_model)
#- D-W 통계량 = 2에 가까울수록 오차항 독립
#- 귀무가설 : 독립이다.

# 잔차분석 - 정규성 (Q-Q plot)
e <- reg_data$y-pre1
qqnorm(e); qqline(e, col="red")

# 잔차분석 - 정규성 (jarque bera)
library(tseries)
jarque.bera.test(resid(reg_model)) # 모델에서 잔차 뽑아내기 : resid(reg_model)
qchisq(0.05,df=2,lower.tail=F) # 5% 유의수준의 임계값
# X-squared = 0.38495 < 5.991465 이기 때문에 정규분포를 따른다.

# 잔차분석 - 등분산성 (잔차 산점도)
reg_data$e<- e
ggplot(reg_data,aes(pre,e))+geom_point(color="gray20",size=2)+
  geom_smooth(method="loess")+labs(x="Predicted",y="잔차")+
  geom_hline(yintercept = 0, color = "red") +theme_classic()+
  ggtitle("잔차산점도")

# 잔차분석 - 등분산성 (Breusch- Pagan Test)
library("lmtest")
bptest(reg_model)
# H0 = 등분산성이 있다. 귀무가설을 기각하므로 등분산성이 아니다.

par(mfrow=c(2,2));plot(reg_model)


#===========================================================
#                      다중공선성
#===========================================================
# vif
library(car)
vif(reg_model)

# condition number
X=model.matrix(reg_model)[,-1]
sqrt(eigen(cor(X))$values[1]/eigen(cor(X))$values)

#===========================================================
#                      영향점/이상점
#===========================================================

#influence measure
influence.measures(reg_model)
influencePlot(reg_model,id.method='identify',main='influence plot', sub='circle size is proportional to Cooks distance')

#===========================================================
#                      변수 선택법
#===========================================================

# 변수선택 - 후진 소거법
step(reg_model,direction="backward")

# 변수선택 - 단계적 선택법
step(reg_model,direction="both")

# 변수선택법 - 전진 선택법 : 전진선택법은 기본모형 선택 필요
reg_model_nothing<-lm(y~1,data=reg_data)
step(reg_model_nothing,direction="forward", scope=(~ x1 + x2))


#===========================================================
#                      모형 선택 기준
#===========================================================

# 모형선택 기준 
AIC(reg_model)
BIC(reg_model)

#===========================================================
#                      추가
#===========================================================
fitness=data.frame(oxygen=c(44.609 , 54.297,  49.874,	45.618,	39.442,
                            50.541,	44.754,	51.855,	40.836,	46.774,
                            39.407,	45.441,	45.118,	45.79,	48.673,
                            47.467,	45.313,	59.571,	44.811,	49.091,
                            60.055,	37.388,	47.273,	49.156,	46.672,	
                            50.388,	46.08,	54.625,	39.203,	50.545,
                            47.92),
                   age=c(44,  44,	38,	40,	44,	44,	45,	54,	51,	48,	57,
                         52,	51,	51,	49,	52,	40,	42,	47,	43,	38,	45,	
                         47,	49,	51,	49,	54,	50,	54,	57,	48),
                   weight=c(89.47,  85.84,	89.02,	75.98,	81.42,	73.03,
                            66.45,	83.12,	69.63,	91.63,	73.37,	76.32,
                            67.25,	73.71,	76.32,	82.78,	75.07,	68.15,
                            77.45,	81.19,	81.87,	87.66,	79.15,	81.42,
                            77.91,	73.37,	79.38,	70.87,	97.63,	59.08,
                            61.24),
                   runtime=c(11.37,  8.65,	9.22,	11.95,	13.08,	10.13,	
                             11.12,	10.33,	10.95,	10.25,	12.63,	9.63,
                             11.08,	10.47,	9.4,	10.5,	10.07,	8.17,	11.63,
                             10.85,	8.63,	14.03,	10.6,	8.95,	10,	10.08,	
                             11.17,	8.92,	12.88,	9.93,	11.5),
                   rstpluse=c(62,  65,	55,	70,	63,	45,	51,	50,	57,	48,	58,	48,
                              48,	59,	56,	53,	62,	40,	58,	64,	48,	56,	47,	44,
                              48,	67,	62,	48,	44,	49,	52),
                   runpluse=c(178,  156,	178,	176,	174,	168,	176,	166,	168,
                              162,	174,	164,	172,	186,	186,	170,	185,	166,
                              176,	162,	170,	186,	162,	180,	162,	168,	156,	
                              146,	168,	148,	170),
                   maxpluse=c(182,  168,	180,	180,	176,	168,	176,	170,	172,	
                              164,	176,	166,	172,	188,	188,	172,	185,	172,	
                              176,	170,	186,	192,	164,	185,	168,	168,	165,	
                              155,	172,	155,	176))

# regression fit                              
R=lm(oxygen~age+weight+runtime+rstpluse+runpluse+maxpluse,data=fitness)
summary(R)

# vif
library(car)
vif(R)

# condition number
X=model.matrix(R)[,-1]
sqrt(eigen(cor(X))$values[1]/eigen(cor(X))$values)

#influence measure
influence.measures(R)
influencePlot(R,id.method='identify',main='influence plot', sub='circle size is proportional to Cooks distance')

#AIC
step(R,direction="backward")

R0 = lm(oxygen~1, data = fitness)
R = lm(oxygen~., data = fitness)

#Forward selection
step(R0, scope = list(lower = R0, upper = R),
     direction = "forward")

#Stepwise selection
step(R0, scope = list(lower = R0, upper = R), 
     direction = "both")


#Boston data
library(MASS)
plot(medv~lstat,Boston)

fit1=lm(medv~lstat,data=Boston)    #simple linear regression
fit1
summary(fit1)
abline(fit1,col="red")

fit2=lm(medv~lstat+age,data=Boston)     #multiple linear regression
summary(fit2)

fit3=lm(medv~.,Boston)  
#multiple linear regression(all variables)
summary(fit3)
par(mfrow=c(2,2)) 
#Resdiual analysis for fit3
plot(fit3)

library(car)
#VIF
vif(fit3)
# condition number
X=model.matrix(fit3)[,-1]
sqrt(eigen(cor(X))$values[1]/eigen(cor(X))$values)

n <- nrow(Boston)
step1 <- step(fit1, medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, direction = "forward")
step2 <- step(fit1, medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, direction = "forward", k=log(n))
step3 <- step(fit3, medv~1, direction = "backward")
step4 <- step(fit3, medv~1, direction = "backward", k = log(n))

R0 = mdev ~ lstat+age
R1 = mdev ~ lstat+age+crim+black+chas
step5 <- step(fit2, list(lower = R0, upper = R1), direction = "both")
step6 <- step(fit2, list(lower = R0, upper = R1), direction = "both", k = log(n))


