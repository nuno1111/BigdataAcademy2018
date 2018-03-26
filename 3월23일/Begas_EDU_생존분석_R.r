#============================================================
# title: "생존분석"
# subtitle: "Survival Analysis"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 백혈병 데이터 설명
#============================================================

#install.packages("survival")

leukemia <- read.csv("3월23일/data.csv")
# leukemia data(출처 : R을 이용한 생존분석 기초, 김재희)

head(leukemia) 

# 백혈병 데이터 : 백혈병 처치그룹과 위약그룹에 따른 재발까지의 시간 데이터
# time1 : 그룹1(처치)에서 재발까지 걸린 시간(단위:주)
# time2 : 그룹2(위약)에서 재발까지 걸린 시간(단위:주)
# status1 : 그룹1(처치)에서 중도절단 여부
# status2 : 그룹2(위약)에서 중도절단 여부

#============================================================
# Kaplan-Meier 생존함수 추정
#============================================================
library(survival)

fit1 <- survfit(Surv(time1, status1)~1, data=leukemia)
fit2 <- survfit(Surv(time2, status2)~1, data=leukemia)

summary1 <- summary(fit1)
summary1
# 백혈병 처치그룹에 대한 Kaplan-Meier 추정결과로 각 시점에서 위험집합, 관측 수, 생존함수, 생존함수 추정에 대한 표준오차, 95% 신뢰하한/상한 


summary2 <- summary(fit2)
summary2
# 백혈병 위약그룹에 대한 Kaplan-Meier 추정결과로 각 시점에서 위험집합, 관측 수, 생존함수, 생존함수 추정에 대한 표준오차, 95% 신뢰하한/상한 


# 평균생존시간 및 표준오차
print(fit1, print.rmean=TRUE)
print(fit2, print.rmean=TRUE)
# rmean : 평균생존시간
# se(rmean) : 평균생존시간의 표준오차
# 처치그룹에서는 평균생존시간이 약 23주이고, 위약그룹에서는 약 9주로 나타남


#============================================================
# Kaplan-Meier 생존곡선
#============================================================

options(repr.plot.width = 5, repr.plot.height = 3)
plot(fit1, conf.int=FALSE,mark.time=TRUE, col="red",xlab="Time(Weeks)", ylab="Survival Probability",
   main="KM-Curves for remission Data") #처치그룹 생존곡선
lines(fit2, conf.int=FALSE, col="blue") #위약그룹 생존곡선
#legend(20,1, legend=c("treatment", "placebe"),lty=c(1:1), col=c("red","blue"))
# 빨간선 : 처치그룹의 생존곡선
# 파란선 : 위약그룹의 생존곡선
# | 표시는 중도절단을 나타냄
# 백혈병 처치그룹이 위약그룹에 비해 생존율이 더 높음을 알 수 있음


#============================================================
# Nelson-Aalen 누적위험함수
#============================================================

nelson.1 <- survfit(Surv(time1,status1)~1, data=leukemia, type="fleming")
nelson.2 <- survfit(Surv(time2,status2)~1, data=leukemia, type="fleming")
summary(nelson.1)
# 백혈병 처치그룹에 대한 Nelson-Aalen 누적위험함수 결과로 각 시점에서 위험집합, 관측 수, 생존함수, 생존함수 추정에 대한 표준오차, 95% 신뢰하한/상한 
summary(nelson.2)
# 백혈병 위약그룹에 대한 Nelson-Aalen 누적위험함수 결과로 각 시점에서 위험집합, 관측 수, 생존함수, 생존함수 추정에 대한 표준오차, 95% 신뢰하한/상한


# 처치그룹에 대한 Nelson-Aalen 누적위험함수
plot(nelson.1$time, -log(nelson.1$surv), xlab="Follow-up time", ylab="Fraction Surviving", ylim=c(0,1.4),
     type="s", main="Nelson-Aalen Estimate of Cumulative Hazard", col="red")
lines(nelson.1$time, -log(nelson.1$upper),type="s", lty=3) #누적위험함수 추정에 대한 95% 신뢰상한
lines(nelson.1$time, -log(nelson.1$lower),type="s", lty=3) #누적위험함수 추정에 대한 95% 신뢰하한
# 점선 : 95% 신뢰상한 및 신뢰하한
# 실선 : 처치그룹의 누적위험함수 그래프



# 위약그룹에 대한 Nelson-Aalen 누적위험함수
plot(nelson.2$time, -log(nelson.2$surv), xlab="Follow-up time", ylab="Fraction Surviving", ylim=c(0,5),
     type="s", main="Nelson-Aalen Estimate of Cumulative Hazard", col="blue")
lines(nelson.2$time, -log(nelson.2$upper),type="s", lty=3) #누적위험함수 추정에 대한 95% 신뢰상한
lines(nelson.2$time, -log(nelson.2$lower),type="s", lty=3) #누적위험함수 추정에 대한 95% 신뢰하한

# 처치그룹과 위약그룹에 대한 Nelson-Aalen 누적위험함수 비교
plot(nelson.1$time, -log(nelson.1$surv), xlab="Follow-up time", ylab="Fraction Surviving", ylim=c(0,5),
     type="s", main="Nelson-Aalen Estimate of Cumulative Hazard", col="red")
lines(nelson.2$time, -log(nelson.2$surv),type="s", col="blue")
# 빨간선 : 처치그룹의 누적위험함수 그래프
# 파란선 : 위약그룹의 누적위험함수 그래프
# 처치그룹에 비해 위약그룹의 누적위험함수가 크고, 시간이 지남에 따라 더욱 큰 차이가 남


#============================================================
# 로그순위검정 (Log-Rank test)
#============================================================

time <- c(leukemia$time1, leukemia$time2)
status <- c(leukemia$status1, leukemia$status2)
group <- c(rep(1,21), rep(2,21)) # 1:처치그룹 2: 위약그룹 => 그룹변수 생성

# Log-Rank 검정
fit <- survdiff(Surv(time, status)~group) # default : rho=0 
fit
# H0: 집단간 생존함수는 같다 H1: 집단간 생존함수는 같지않다
# 카이제곱 통계량은 16.8이고 p-value도 유의수준 0.05 보다 작으므로 귀무가설을 기각
# 따라서 유의수준 5%에서 처리집단 간 생존함수가 다르다고 할 수 있음


# Gehan-Wilcoxon 검정
fit.g <- survdiff(Surv(time,status)~group, rho=1) #option (rho=1 : Gehan test)
fit.g
# H0: 집단간 생존함수는 같다 H1: 집단간 생존함수는 같지않다
# 카이제곱 통계량은 14.55이고 p-value도 유의수준 0.05 보다 작으므로 귀무가설을 기각
# 따라서 유의수준 5%에서 처리집단 간 생존함수가 다르다고 할 수 있음

#============================================================
# Cox 비례위험모형
# (Q)유의한 공변량은 무엇이며 공변량의 영향력을 어떻게 해석할까?
#============================================================
# install.packages("GlobalDeviance")
library(GlobalDeviance)
data(Rossi)
Rossi <- Rossi[,c(1,2,3,4,5,6,7,8,9,10)]

# Rossi Data(출처: R을 이용한 생존분석 기초, 김재희 )
head(Rossi)
# Rossi 자료 : 남자 범죄자들을 대상으로 한 상습적 범행에 대한 실험 연구
# week : 출소 후 처음으로 체포된 주 또는 중도절단된 기간
# areest : 사건발생여부 (1:다시 체포, 0:그렇지 않음)
# fin : 가변수로 출소 후 재정적지원여부 (1:받음 0:받지않음)
# age : 출소 시 나이
# race : 흑인 (1:그렇다 0:아니다)
# wexp : 투옥시 전일제 일 (1:있음 0:없음)
# mar : 결혼여부 (1:기혼 0:미혼)
# paro : 가석방여부 (1:그렇다 0:아니다)
# prio : 사전범죄횟수
# edu : 교육정도


# Cox 비례위험모형 - 모든 변수 포함
cox.r1 <- coxph(Surv(week, arrest)~fin+age+race+wexp+mar+paro+prio+educ, data=Rossi)

# 유의수준 10%에서 유의한 공변량은 fin(재정적지원 여부), age(출소한 나이),prio(사전범죄횟수)
# 모형적합도 검정 : Likelihood ratio test, Wald test, Score test 결과 모두 p-값이 매우 작아 모형적합도는 유의
# 위험률 해석
# fin 이 1단위 증가할 때, 0.698배 위험률 증가(재정지원이 있는 경우에 위험률 감소)
# age 가 1단위 증가할 때, 0.944배 위험률 증가(나이가 증가함에 따라 위험률 감소)
# prio가 1단위 증가할 때, 1.088배 위험률 증가(사전범죄횟수에 따라 위험률 증가)


# Cox 비례위험모형 - 선택된 변수만
cox.r2 <- coxph(Surv(week, arrest)~fin+age+prio, data=Rossi)
summary(cox.r2)

# 위험률 해석
# fin이 1단위 증가할 때, 0.7068배 위험률 증가(재정지원이 있을수록 재수감 위험률 감소)
# age가 1단위 증가할 때, 0.9351배 위험률 증가(나이가 증가함에 따라 재수감 위험률 감소)
# prio가 1단위 증가할 때, 1.1017배 위험률 증가(전적 투옥횟수에 따라 위험률 증가)

#============================================================
# (Q)공변량들이 비례위험가정을 만족할까?
#============================================================

# 비례위험가정 검정
cox.zph(cox.r2) 
# H0 : 비례성을 만족 H1: 비례성을 만족하지 않음
# age에서는 유의수준 5%에서 비례성을 기각함
# fin과 prio에 대해서는 비례성을 기각하지 못함
# Global 검정은 사용한 공변량 모두에 대한 것으로 유의수준 0.05에서 비례성을 기각하지 못함(따라서 비례성을 만족함)


# Schoenfeld 잔차 산점도
plot(cox.zph(cox.r2)) 

# Schoenfeld 잔차그림 : 각 공변량에 대해 모형적합 후 잔차¶
# 실선 : 스플라인 스무딩한 것, 점선 : +2,-2배 표준오차 범위
# 스무딩한 선이 수평선에서 벗어날수록 비례위험모형의 부적합성을 나타냄
# fin과 prio는 비례위험모형 가정을 만족해보임
# age는 시간에 따라 감소 경향이 보임

