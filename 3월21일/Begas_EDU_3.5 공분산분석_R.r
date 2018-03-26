

#============================================================
# title: "공분산분석"
# subtitle: "Analysis of Covariance"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# Cricket 데이터 설명
#============================================================

Data <- read.table("3월21일/data_anc.txt", header=TRUE)
head(Data)

# Cricket Data
# species : 종류 (ex/niv/fake)
# Temp : 온도
# Pulse : 파동


options(repr.plot.width = 5, repr.plot.height = 5)

plot(x = Data$Temp, y =Data$Pulse, col = Data$Species, pch = 16, xlab = "Temperature", ylab = "Pulse")
legend('bottomright', legend = levels(Data$Species), col = 1:3, cex=1, pch=16)

# 종속변수(Pulse)와 공변량(Temperature) 사이에서 선형관계를 보이는 것을 확인 할 수 있음
# 따라서 종속변수와 공변량의 선형성 가정을 만족함

#============================================================
# 공분산분석 해석
#============================================================

options(contrasts = c("contr.treatment", "contr.poly"))
model.1 = lm(Pulse ~ Temp + Species + Temp:Species, data=Data)

library(car)
Anova(model.1, type="II")

# 온도와 종류의 상호작용효과가 유의하지 않고, 따라서 각각의 그룹의 기울기는 다르지 않음
# Temp:Species 의 p-value=0.4093 이므로 유의하지 않음


#============================================================
# 공분산분석 해석 (상호작용 변수 제외하고)
#============================================================

model.2 = lm(Pulse ~ Temp + Species, data = Data)

library(car)

Anova(model.2, type="II")

# 범주형변수인 Species는 유의하고 따라서 각 그룹의 절편은 다름
# Species의 p-value는 2.252680e-40 이므로 유의함

summary(model.2)

# 기울기 = Temp의 Estimate
# species 1(ex)의 절편 = (intercept)
# species 2(fake)의 절편 = (intercept)+Speciesfake
# species 3(niv)의 절편 = (intercept)+Speciesniv


#============================================================
# 공분산분석 가정 확인
#============================================================

I0 = -6.35729
I1 = I0 + 0
I2 = I0 + 19.81429 
I3 = I0 + -10.18571 
B  = 3.56961 

plot(x = Data$Temp, y = Data$Pulse, col = Data$Species, pch = 16, xlab = "Temperature", ylab = "Pulse")

legend('bottomright', legend = levels(Data$Species), col = 1:3, cex = 1, pch = 16)

abline(I1, B, lty=1, lwd=2, col = 1)
abline(I2, B, lty=1, lwd=2, col = 2)
abline(I3, B, lty=1, lwd=2, col = 3)

# 1)종속변수와 공변량의 선형
# 2)회귀계수의 동일성 => 각 그룹의 기울기가 같다
# 1)과 2)의 가정을 만족함을 확인할 수 있음


hist(residuals(model.2), col="darkgray")

# 잔차의 히스토그램을 확인해보면, 잔차의 분포가 근사적으로 정규분포를 따름을 확인 할 수 있음

plot(fitted(model.2), residuals(model.2))

#  잔차와 예측값에 대한 플롯으로 잔차가 편향되어 있지 않음을 확인 할 수 있음 


#============================================================
# Anorexia 데이터 설명
#============================================================

library(MASS)
data(anorexia)
anorexia$Diff <- anorexia$Postwt-anorexia$Prewt
head(anorexia)
str(anorexia)

# anorexia(거식증) data
# Treat : 치료법 (CBT / Cont / FT)
# Prewt : 치료전 몸무게
# Postwt : 치료후 몸무게
# Diff : 치료전후의 차이

levels(anorexia$Treat)  
anorexia$Treat <- relevel(anorexia$Treat, ref="Cont") 
levels(anorexia$Treat) 
anorexia$Treat <- factor(anorexia$Treat, levels=c("Cont","CBT","FT")) 

# Cont가 대조군이고 CBT와 FT가 비교군 이므로 relevel()을 Cont를 앞으로 바꿔줌

out <- lm(Postwt~Prewt+Treat, data=anorexia) # lm(y~공변량변수 + 그룹변수)
anova(out)   
summary(out)

# p-value가 0.00084로 치료효과의 차이가 있음
# CBT와 FT를 reference인 Cont와 비교를 했을 때, p-value 값을 보면 두 치료법에서 모두 유의한 차이가 있다고 나타남
# CBT의 p-value는 0.0339 FT의 p-value는 0.0001로 유의수준 0.05보다 작으므로 유의하다고 할 수 있음

#============================================================
# 공분산분석 가정 확인
#============================================================

sumRes <- summary(out)
coeffs    <- coef(sumRes)
beta_0 <- coeffs[1, 1]
beta_1 <- coeffs[3, 1] + beta_0
beta_2   <- coeffs[4, 1] + beta_0
slopeAll  <- coeffs[2, 1]

plot(anorexia$Prewt, anorexia$Postwt, col=anorexia$Treat, xlab = "Prewt", ylab = "Postwt")
abline(beta_0, slopeAll, col="green")
abline(beta_1, slopeAll, col="red")
abline(beta_2,   slopeAll, col="blue")

# 1) 종속변수(Postwt)와 공변량(Prewt)간의 선형관계를 보임 
# 2) 세 그룹의 회귀계수는 동일함 
# 공분산분석 기본가정에서 2가지 만족함을 확인 


# 오차항의 정규성 
hist(residuals(out), col="darkgray")

# 잔차의 정규성 가정 확인 

#============================================================
# 다중비교
#============================================================
# install.packages("multcomp")
library(multcomp)
dunnett <- glht(out, linfct=mcp(Treat="Dunnett")) 
summary(dunnett)

# 한개의 대조군(Cont)을 두개의 비교군(CBT/FT)과 비교하므로 Dunnett의 방법으로 다중비교
# FT는 Cont와 유의한 차이가 있지만, CBT는 Cont와 유의한 차이가 없음
#(FT의 p-value는 0.00037 CBT의 p-value는 0.062939 이므로)