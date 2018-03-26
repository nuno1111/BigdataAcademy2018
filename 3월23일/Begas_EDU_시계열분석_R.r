#============================================================
# title: "시계열 분석"
# subtitle: "Time Series Analysis"
# author: "Begas"
# date: "2018"
#============================================================

# install.packages("forecast")
# install.packages("tseries")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("zoo")

library(forecast)
library(tseries)
library(ggplot2)
library(reshape)
library(zoo)

#============================================================
# Data Load
# - 1949년 ~ 1960년 까지의 월별 비행기 탑승 고객 수
#============================================================ 
origin <- AirPassengers
origin

#============================================================
# EDA
#============================================================ 

# 시도표
plot(origin)
# 데이터에 이분산이 존재함을 확인일 수 있음
# 분산 안정화를 위한 데이터 변환 필요

#분산 안정화를 위한 BoxCox 변환
lambda <- BoxCox.lambda(origin)
tran_org <- BoxCox(origin, BoxCox.lambda(origin))
plot(tran_org)
# BoxCox 변환 이후 이분산의 효과가 줄어 든것을 확인

# 정규성 및 Corr
# Hist Plot
hist(tran_org,prob=TRUE,12)
lines(density(tran_org))
# Q-Q PLOT
qqnorm(tran_org)
qqline(tran_org)

# 상관관계 확인
lag.plot(tran_org,12,do.lines=FALSE) # 시간차에 따른 상관관계/자기상관관계???
#전반적으로 데이터는 정규분포를 따르고 시차가 12일때 상관관계가 높음

#============================================================
# 시계열 분해 및 회귀분석 이용 예측
#============================================================ 
stl_tran_org <- stl(tran_org, s.window = 12)

plot(stl_tran_org)
# 1차 Trend와 Seasonality 존재
# 잔차는 White Noise로 판단

# 계절형 Dummy 변수 생성
M <- factor(cycle(tran_org))
stl_tran_org_df <- as.data.frame(stl_tran_org$time.series)

# 회귀 모형 생성
# 모형식 : tran_org=trend∗β1+M1∗d1+...+M11∗d11+ϵ
model_stl <- lm(formula = tran_org~ 0+stl_tran_org_df$trend+M, na.action = NULL)
summary(model_stl)

# 잔차 검정
# time Plot
plot(resid(model_stl))
# Hist Plot
hist(resid(model_stl),prob=TRUE,12)
lines(density(resid(model_stl)))
# Q-Q PLOT
qqnorm(resid(model_stl))
qqline(resid(model_stl))
# Q-Q Plot과 Histogram을 확인하면 양쪽 끝이 두텁지만 White Noise라고 판단하기 어려움이 없음

plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')

# 기존 데이터 및 fitted 데이터의 비교
# BoxCox 역변환 필요 함
lines(InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)), col='red')
mean((origin - InvBoxCox(model_stl$fitted.values, lambda = BoxCox.lambda(origin)))^2, na.rm = TRUE)

#============================================================
# 지수평활을 이용한 예측
#============================================================

plot(stl(origin, s.window=12))
# Trend 및 Seasonality 존재
# Holt-Winter 지수평활 모형이 적합

#HoltWinters 모형 생성
model_es <- HoltWinters(origin, seasonal = "multiplicative")

# 원 데이터 및 fitted 데이터의 비교
# plot
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(model_es$fitted[,1], col='red')

# mse 125.5
mean((origin-model_es$fitted[,1])^2)

# 예측
p <- predict(model_es, 60, prediction.interval = TRUE)
plot(model_es, p)

#============================================================
# ARIMA를 이용한 예측
#============================================================

# 데이터 탐색 및 모형식별
# 시도표
plot(origin)
# 데이터의 이분산과 1차 추세가 존재함
# 분산 안정화를 위한 Box Cox 변환과 1차 차분 필요

# 분산 안정화 및 차분
tran_org <- BoxCox(origin, BoxCox.lambda(origin))
plot(tran_org)
tran_diff_org <- diff(tran_org)
plot(tran_diff_org)

# ACF, PACF를 통한 탐색
acf(tran_diff_org, lag.max=24)
pacf(tran_diff_org, lag.max=24)

# 계절 차분 및 ACF, PACF를 통한 탐색
tran_sdiff_org <- diff(tran_diff_org, lag = 12)
plot(tran_sdiff_org)

acf(tran_sdiff_org, lag.max = 24)
# acf는 lag=1,3,12에서 0이 아닌값 가짐  비계절 시차 4부터 절단 -> MA(3), 계절 -> 시차 2부터 절단 SMA(1)

pacf(tran_sdiff_org, lag.max = 24)
# 시차 2와 8에서 0보다 큰 값을 가지지만 정확한 모형을 찾기 위해 auto.arima를 통해 aic가 최소가 되는 order 값 구함

auto.arima(tran_sdiff_org, max.p = 3, max.q=3, max.Q=1)

# 모형 구축
model_arima <- arima(tran_org, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

# 모형 검진
# 잔차 검정
tsdiag(model_arima)
# 독립성 검정
Box.test(model_arima$residuals, type="Ljung-Box")
# 잔차의 독립성, 등분산성, 정규성 만족

# 원 데이터 및 fitted 데이터의 비교
plot(spline(time(origin), origin),type='l',xlab='Time',ylab='Pop')
lines(InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)), col='red')
mean((origin - InvBoxCox(fitted(model_arima), BoxCox.lambda(origin)))^2)

# 12개월 예측
arima_fit <- predict(model_arima, n.ahead=12) #BoxCox 변환 데이터 사용
lambda <- BoxCox.lambda(origin)
ts.plot(origin, xlim=c(1950,1965), ylim = c(0, 1000))
lines(InvBoxCox(arima_fit$pred, lambda),col="red")
lines(InvBoxCox(arima_fit$pred+1.96*arima_fit$se, lambda),col="blue",lty=1)
lines(InvBoxCox(arima_fit$pred-1.96*arima_fit$se, lambda),col="blue",lty=1)

# test
auto.arima(origin)
model_arima <- arima(tran_org, order=c(2,1,1), seasonal = list(order = c(0,1,0), period = 12))
plot(forecast(model_arima,h=120))

model_arima2 <- arima(tran_org, order=c(2,2,2), seasonal = list(order = c(2,2,2), period = 12))
plot(forecast(model_arima2,h=120))


