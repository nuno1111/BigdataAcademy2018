#============================================================
# title: "로지스틱회귀분석"
# subtitle: "Logistic Regression Analysis"
# author: "Begas"
# date: "2018"
#============================================================
par(mfrow = c(2,1))
plot(function(x) dbinom(x, 10, prob=1/2), 0, 10, type = "h")
plot(function(x) dnorm(x), -4 , 4)


#============================================================
# 데이터 설명
#============================================================

# 남미의 심장병 발생 확률이 높은 지역의 남성에 대한 retrospective study를 한 데이터
# install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
heart <- SAheart[,c(2,5,8:10)]
head(heart,4)

# tobacco: 누적 흡연량
# famhist: 유전도
# alcohol:알코올
# age:나이
# chd: 심장병 유무


# 심장병 유무 v.s 흡연량
library(ggplot2)
ggplot(SAheart, aes(x = factor(chd), y = tobacco, fill = chd)) + geom_boxplot()

# 심장병 유무 v.s 알코올
ggplot(SAheart, aes(x = factor(chd), y = alcohol, fill = chd)) + geom_boxplot()

# 심장병 유무 v.s 나이
ggplot(SAheart, aes(x = factor(chd), y = age, fill = chd)) + geom_boxplot()

#============================================================
# 모형 적합
#============================================================

heartfit <- glm(chd ~ ., data = heart, family = binomial)
round(coef(summary(heartfit)), 3)
# tobacco, famhistPresent, age는 p-value<0.05이므로 유의수준 5%에서 유의
# alcohol변수는 p-value가 0.984로 0.5보다 크므로 유의수준 5%에서 유의하지 않음

# 변수선택
new_heartfit <- step(heartfit)
# alcohol변수가 제거됨

# LR test
anova(new_heartfit, heartfit, test = "LRT")
# 두 모형이 차이가 없으므로 새로운 모형을 채택

# 회귀계수 추정
round(coef(summary(new_heartfit)), 3)[, 1]

# tobacco가 한단위 증가하면 심장병에 걸릴 오즈가 exp(0.08) 증가
# famhistPresent가 한단위 증가하면 심장병에 걸릴 오즈가 exp(0.97) 증가
# age가 한단위 증가하면 심장병에 걸릴 오즈가 exp(0.048) 증가

#============================================================
# 모형 평가
#============================================================
heartfit$aic

# cut-off가 0.7기준일 경우
y.pred1 = ifelse(heartfit$fitted.values > .7, 1, 0)

table(heart$chd, y.pred1)

# 정분류율
round(mean(heart$chd == y.pred1),2)

# cut-off가 0.5기준일 경우
y.pred2 = ifelse(heartfit$fitted.values > .5,1,0)
table(heart$chd, y.pred2)
# 정분류율
round(mean(heart$chd==y.pred2),2)

# cut-off가 0.3기준일 경우
y.pred3 = ifelse(heartfit$fitted.values > .3, 1, 0)
table(heart$chd, y.pred3)
# 정분류율
round(mean(heart$chd == y.pred3),2)

# 여러 cut-off-value에 대해 시도해 본 결과 정분류율이 가장 높은 0.5를 cut-off value로 채택


indexes <- sample(1:nrow(SAheart), size=0.2*nrow(SAheart))
indexes
# Split data
test <- SAheart[indexes,]
train <- SAheart[-indexes,]
nrow(test)

fit_rs <- predict(heartfit, newdata = SAheart[,-10], type="response")
fit_rs


#===========================================================
#                   추가
#===========================================================
# 로지스틱 회귀모형 예
data <- data.frame(sex = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
                   FSIQ = c(149,139,133,89,133,141,135,100,80,83,97,139,141,103,144,133,137,99,138,92,132,140,96,83,132,101,135,91,85,77),
                   weight = c(166,143,172,134,172,151,155,178,180,166,186,132,171,187,191,118,147,146,138,175,118,155,146,135,127,136,122,114,140,106),
                   height = c(72.5,73.3,68.8,66.3,68.8,70.0,69.1,73.5,70.0,71.4,76.5,68.0,72.0,77.0,67.0,64.5,65.0,69.0,64.5,66.0,64.5,70.5,66.0,68.0,68.5,66.3,62.0,63.0,68.0,63.0))


# 로지스틱 회귀모형식
logistic.fit <- glm(factor(sex) ~ FSIQ + weight + height, 
                    data = data, family = binomial)


step(direction='backward',logistic.fit)


# 분류표
pred_sex <- predict(logistic.fit, newdata = data[, -1], type = "response")

cut <- ifelse(pred_sex >= 0.5, 2, 1) ## 절단값 0.5로 설정
result <- table(data$sex, cut)

# 정분류율
accuracy <- (result[1, 1] + result[2, 2]) / sum(result) * 100



## logistic regression
library(ElemStatLearn)
data(SAheart)
heart = SAheart[,c(1:3,5,7,9,10)]

heartfit  = glm(chd~., data = heart, family = binomial)
summary(heartfit)

# how to decide the cut-off value
y.pred1 = ifelse(heartfit$fitted.values>.7,1,0)
y.pred2 = ifelse(heartfit$fitted.values>.5,1,0)
y.pred3 = ifelse(heartfit$fitted.values>.3,1,0)

table(heart$chd, y.pred1)
table(heart$chd, y.pred2)
table(heart$chd, y.pred3)

#===========================================================
#                   변수선택
#===========================================================
#전진선택법

#install.packages("ElemStatLearn")
library(ElemStatLearn)
Data <- prostate[,-ncol(prostate)]

#전진 선택법은 상수항만 포함된 모형(lm.const)에서 출발
lm.const <- lm(lpsa~1, data=Data)

#AIC를 이용한 전진 선택법
forward.aic <- step(lm.const, lpsa~lcavol + lweight 
                    + age + lbph + svi + lcp + gleason 
                    + pgg45, direction="forward")

#선택된 최종 모형
summary(forward.aic)


#후진 소거법
#후진 소거법은 모든 변수가 포함된 모형(lm.full)에서 출발
lm.full <- lm(lpsa~., data=Data)

#AIC를 이용한 후진 소거법
backward.aic = step(lm.full, lpsa~1, direction="backward")

#선택된 최종 모형
summary(backward.aic)

#단계적 선택법
#단계적 선택법은 적절한 모형에서 출발
#여기서는 상수항만 포함된 모형(lm.const)에서 출발
#AIC를 이용한 단계적 선택법
stepwise.aic <- step(lm.const, lpsa~lcavol + lweight 
                     + age + lbph + svi + lcp + gleason 
                     + pgg45, direction="both")

#선택된 최종 모형
summary(stepwise.aic)
