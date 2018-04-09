#============================================================
# title: "일반화 선형모형"
# subtitle: "Generalized linear model"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 패키지 설치
#============================================================
#install.packages("faraway")
library(faraway)

#============================================================
# 포아송 회귀분석
#============================================================

#============================================================
# 데이터 설명
#============================================================

# (Q) Galapagos 섬들의 식물 수에 어떤 요인이 영향을 끼칠까?
data(gala)
gala <- gala[, -2]
head(gala)
# Species: 섬에 있는 식물 종류의 수
# Endemics: 토종 식물 수
# Area: 섬의 면적
# Elevation: 섬의 최고 고도
# Nearest: 가장 가까운 섬의 거리
# Scruz: Santa Cruz 섬의 거리
# Adjacent: 인접한 섬의 면적

# plot
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

pairs(gala, lower.panel = panel.cor, upper.panel = upper.panel)

#============================================================
# 모형 적합
#============================================================
gala_glm <- glm(Species ~ ., family = poisson, data = gala)

#============================================================
# 모형 해석
#============================================================
summary(gala_glm)
# 모든 변수가 유의함

round(exp(coef(summary(gala_glm))[, 1]), 4)
# Area가 한단위 증가하면 식물의 수가 0.9998 증가
# Elevationt가 한단위 증가하면 식물의 수가 1.0076 증가
# Nearest가 한단위 증가하면 식물의 수가 1.015 증가
# Scruz가 한단위 증가하면 식물의 수가 0.9995 증가
# Adjacent가 한단위 증가하면 식물의 수가 0.9987 증가

anova(gala_glm, test = "Chi")

#============================================================
# 모형 비교
#============================================================
# 임의로 Nearest 변수를 제거한 후 모형 적합
gala_glm2 <- glm(Species ~ . - Nearest, family=poisson, data=gala)
anova(gala_glm, gala_glm2, test="Chi")
# p-value가 매우 작으므로 두 모형은 차이가 있다.
# 따라서 Nearest 변수를 제거하지 않은 모형을 최종 모형으로 선택

#============================================================
# 모형 진단
#============================================================
# 잔차 분석
gala_res <- cbind(residuals(gala_glm), residuals(gala_glm,"pearson"), residuals(gala_glm, "response"))
colnames(gala_res) <- c("Deviance","Pearson","Response")
head(gala_res)

# 이탈도 잔차 v.s 예측치
gala_resi_df <- data.frame(x = predict(gala_glm, type="link"), y = residuals(gala_glm))

library(ggplot2)
ggplot(aes(x = x, y = y), data = gala_resi_df) + 
  geom_point(col="orange") + labs(x = expression(hat(eta)), y = "Deviance residuals")

gala_resi_df <- data.frame(x = predict(gala_glm, type="link"), y = residuals(gala_glm, type="response"))
ggplot(aes(x = x, y = y), data = gala_resi_df) + geom_point(col="orange") + labs(x = expression(hat(eta)), y = "Response residuals")
# 그림을 통해 잔차가 일정하게 증가하는 패턴을 확인 가능

# Q-Q plot
halfnorm(rstudent(gala_glm))
# 분포의 가정 확인
# 이상치 확인

#============================================================
# 음이항 분포
#============================================================

# 포아송 분포: 평균 = 분산
# 음이항 분포: 평균 < 분산
mean(gala$Species)
var(gala$Species)
# 평균에 비해 분산이 상대적으로 크므로 음이항 분포 적용

#============================================================
# 모형 적합
#============================================================
library("MASS")
gala_nb1 <- glm(Species ~., family=negative.binomial(1), data = gala)
summary(gala_nb1)

gala_nb2 <- glm.nb(Species ~., data = gala) # 최적의 모수 선택
summary(gala_nb2)
