#============================================================
# title: "T-검정"
# subtitle: "T-Test"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 단일 표본 T-검정 
# - (Question) 과자의 무게가 350g라고 한다. 이 무게가 정확한것인지 과자 10봉지를 구매 후 통계적 검정을 해보자
# - 데이터 : 과자무게 (단위: 점)
# - 가설 
#        H_0 : mu_과자무게 = 350 vs H_1 : Not H_0
# - 유의수준 : 0.05
#============================================================

#데이터 수집
onesample <- read.csv("3월20일/one.csv", header = T)


#데이터 조회
head(onesample)

#단일 표본 T-검정 수행
t.test(onesample, alternative = "two.sided", mu = 350)
t.test(onesample, alternative = "less", mu = 350)
# p-value < 0.005 이므로 유의수준 0.01 하에 귀무가설 기각
# 과자의 평균 무게가 350g이라고 할 수 없음

#============================================================
# 독립 이표본 T-검정
# - (Question) 남자와 여자의 과자 선호도 점수 차이가 있을까?
# - 데이터 : 남자선호도, 여자선호도 (단위: 점)
# - 가설 
#         H_0 : mu_남 = mu_여 vs H_1 : Not H_0
# - 유의수준 : 0.05
#============================================================

#데이터 수집
two_sample <- read.csv("3월20일/two.csv", header = T)

#데이터 조회  					 
head(two_sample)

#이표본 T-검정 수행
t.test(score ~ gender, alternative = "two.sided", data = two_sample)
t.test(score ~ gender, alternative = "less", data = two_sample)
# p-value < 0.005 이므로 유의수준 0.01 하에 귀무가설 기각
# 남/여의 평균 선호도 점수는 다르다고 할 수 있음


#============================================================
# 쌍체표본 T-검정
# - (Question) 과자를 먹기 전/후 체중 차이가 있을까?
# - 데이터 : before(과자 먹기 전 체중), after(과자 먹은 후 체중) (단위: kg)
# - 가설 
#        H_0 : mu_before - mu_after = 0 vs H_1 : Not H_0
# - 유의수준 : 0.05
#============================================================

#데이터 수집
paired_sample <- read.csv("3월20일/paired.csv", header = T)

#데이터 조회
head(paired_sample)

#쌍체 표본 T-검정 수행
t.test(paired_sample$before, paired_sample$after , paired = T)
# p-value < 0.005 이므로 유의수준 0.01 하에 귀무가설 기각
# 과자를 먹기 전/후의 평균 무게 차이가 있다고 할 수 있음
