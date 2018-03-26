#============================================================
# title: "분산분석"
# subtitle: "Analysis of Variance"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 데이터 설명
#============================================================

#(Q) 살충제(A~F)의 효과가 같을까?
data(InsectSprays)
head(InsectSprays)
#spray: 살충제의 종류(A~F)
#count: 곤충의 수

library(ggplot2)
ggplot(InsectSprays, aes(x=spray, y=count, fill=spray)) + 
  geom_boxplot()
# 상자그림을 그렸을 때 살충제 별로 효과가 다르다는 것을 볼 수 있다.

#============================================================
# 일원배치법
#============================================================
aov.result <- aov(count~spray, data=InsectSprays)

# 분산분석표
summary(aov.result)
# 유의확률이 매우 작으므로(<2e-16) 유의수준 0.05에서 귀무가설을 기각하여 각 살충제의 효과가 다르다.

par(mfrow=c(2,2))
plot(aov.result)


#편두통을 위한 세가지 약이 효과가 서로 같은지에 대한 실험을 하기 위해 
#편두통이 있는 27명의 환자를 랜덤하게 3개의 그룹으로 나누고, 
#두통이 있을 때 약을 먹은 후 두통의 정도를 1부터 10까지 로 표현한 데이터

pain = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug = c(rep("A",9), rep("B",9), rep("C",9))
migraine = data.frame(pain,drug)

plot(pain ~ drug, data=migraine)

migraine_aov <- aov(pain ~ drug, data = migraine)

summary(migraine_aov)

#============================================================
# 이원배치법
#============================================================

#(Q) 화학공정에서 온도와 압력의 효과가 같을까?

temppressure_df <- read.csv("3월21일/temppressure.csv")
temppressure_df$pressure <- as.factor(temppressure_df$pressure)
head(temppressure_df)
# pressure: 압력(200, 220, 240)
# temp: 온도(low, high)

# 상자그림
ggplot(temppressure_df, aes(x=pressure, y=y, fill=pressure)) + 
  geom_boxplot()
# 그림만으로 판단하기 어렵다.

ggplot(temppressure_df, aes(x=temp, y=y, fill=temp)) + 
  geom_boxplot()
# 온도에 따른 차이가 있다.

# 교호작용 그림
with(temppressure_df,{
  interaction.plot(temp,pressure,y,bty='l', main='interaction plot') 
})
with(temppressure_df,{
  interaction.plot(pressure,temp,y,bty='l', main='interaction plot') 
})

# Interaction plot에서 선분들이 평행하면 교호작용이 완전히 없는 상태인데 주어진 plot으로 보면 교호작용이 강하진 않음을 알 수 있다.

# 분산분석표
aov.result <- aov(y~temp+pressure+temp:pressure,data=temppressure_df)
summary(aov.result)
# 검정결과 유의수준 5%에서 온도의 차이만 유의하다.

