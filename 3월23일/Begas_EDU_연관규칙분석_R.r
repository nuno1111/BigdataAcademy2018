#============================================================
# title: "연관규칙분석"
# subtitle: "Assciation Rule"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 패키지 설치
#============================================================

# install.packages("arules")
# install.packages("arulesViz")
library(arules)
library(arulesViz)

#============================================================
# 데이터 설명 : Adult data
#============================================================

# 미국 Census Bureau의 Census Income데이터 베이스에 추출한 설문조사 자료
# 관측치의 개수 : 48843개
# 나이, 직업군, 교육정도 등의 주로 범주형인 15개의 변수 포함

# (Q) 어떤 규칙을 갖는 사람들이 남성일까?
data(Adult)
summary(Adult)


# 지지도(support)가 0.4이상인 item들의 빈도 막대그래프
itemFrequencyPlot(Adult, support = 0.1, main = "Item Frequency Plot above support 0.4")
# 지지도 기준 상위 10개 item들의 빈도 막대그래프
itemFrequencyPlot(Adult, topN = 10, main = "Histogram of support top 10 items")

#============================================================
# 연관규칙분석
#============================================================

# 지지도가 0.4 이상인 연관규칙
rules <- apriori(Adult, parameter = list(support = 0.1))
summary(rules)
inspect(rules)

# 지지도가 0.4이상이면서 향상도가 1.3 이상인 것
rules.sub <- subset(rules, subset = rhs %pin% "sex" & lift > 1.3)
inspect(sort(rules.sub)[1:3])
# 위 결과로는 “시민권자와 결혼하였으면 남성이다.” , “남편이면 남성이다”, “시민권자와 결혼했고 남편이면 남성이다.”라는 것들을 찾음

# 모든 규칙의 산점도
# x축: 지지도, y축: 향상도, 점의 색: 신뢰도
plot(rules, measure = c("support", "lift"), shading = "confidence")

# 같은 그래프를 3D로 표현
# control=list(reorder=TRUE) 옵션: 아이템들을 재배열-> 군집효과를 두드러지게 함
plot(rules, method = "matrix3D", measure = "lift", control = list(reorder = TRUE))

# 향상도 기준 상위 10개의 연관 규칙 시각화
# 색깔: 향상도의 크기, 원의 크기: 지지도의 크기
subrules2 <- head(sort(rules, by = "lift"), 10) 
plot(subrules2, method = "graph")



