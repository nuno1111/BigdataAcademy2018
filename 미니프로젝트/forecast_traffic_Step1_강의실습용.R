##### [ Ⅱ. 교통량을 사용한 예측 분석 ]
#### 1. 데이터 수집 및 로드
### 2) 데이터 로드
### - 교통량 데이터 로드
kimpo2012 <- read.csv(file = "./data/transportation/4803-002_2012.csv", fileEncoding = "euc-kr")
kimpo2013 <- read.csv(file = "./data/transportation/4803-002_2013.csv", fileEncoding = "euc-kr")
kimpo2014 <- read.csv(file = "./data/transportation/4803-002_2014.csv", fileEncoding = "euc-kr")
kimpo2015 <- read.csv(file = "./data/transportation/4803-002_2015.csv", fileEncoding = "euc-kr")
kimpo2016 <- read.csv(file = "./data/transportation/4803-002_2016.csv", fileEncoding = "euc-kr")

### 3) 데이터 병합
### - 컬럼명 확인
rbind(colnames(kimpo2012), colnames(kimpo2013), 
      colnames(kimpo2014), colnames(kimpo2014), colnames(kimpo2016))

### - 데이터 병합
kimpo <- rbind(kimpo2012, kimpo2013, kimpo2014, kimpo2015, kimpo2016)
rm(kimpo2012, kimpo2013, kimpo2014, kimpo2015, kimpo2016) #기존 5년치 데이터셋 삭제 

#### 2. 원시데이터에 대한 품질 체크
### 1) 데이터 특성 탐색
### - 데이터 앞부분 확인 (전체 기준)
head(kimpo, 5)

### - 데이터 앞부분 확인 (방향 기준)
by(kimpo, kimpo$방향, head, 5)

### - 데이터 뒷부분 확인 (전체 기준)
tail(kimpo, 5)

### - 데이터 뒷부분 확인 (방향 기준)
by(kimpo, kimpo$방향, tail, 5)

### - 별도 창에서 데이터 확인 (전체 기준)
View(kimpo)

### - 별도 창에서 데이터 확인 (방향 기준)
by(kimpo, kimpo$방향, View)

### - 데이터 크기 확인 (전체 기준)
dim(kimpo)

### - 데이터 크기 확인 (방향 기준)
by(kimpo, kimpo$방향, dim)

### - 데이터 행 개수 확인 (전체 기준)
length(kimpo)

### - 데이터 행 개수 확인 (방향 기준)
by(kimpo,kimpo$방향, length)

### - 데이터 열 개수 확인 (전체 기준)
nrow(kimpo)

### - 데이터 행 개수 확인 (방향 기준)
by(kimpo,kimpo$방향, nrow)

### - 데이터셋 속성 확인
attributes(kimpo)

### - 변수 속성 확인
str(kimpo)

### 2) 변수 특성 탐색
### 2-1) 통계량 산출
### - 특정 데이터 영역 선택
kimpo_up <- kimpo[kimpo$방향 == '상행',]
kimpo_down <- kimpo[kimpo$방향 == '하행',]
kimpo_up2012 <- kimpo[kimpo$방향 == '상행' & kimpo$년도 == '2012',]
kimpo_up2013 <- kimpo[kimpo$방향 == '상행' & kimpo$년도 == '2013',]
kimpo_up2014 <- kimpo[kimpo$방향 == '상행' & kimpo$년도 == '2014',]
kimpo_up2015 <- kimpo[kimpo$방향 == '상행' & kimpo$년도 == '2015',]
kimpo_up2016 <- kimpo[kimpo$방향 == '상행' & kimpo$년도 == '2016',]
kimpo_down2012 <- kimpo[kimpo$방향 == '하행' & kimpo$년도 == '2012',]
kimpo_down2013 <- kimpo[kimpo$방향 == '하행' & kimpo$년도 == '2013',]
kimpo_down2014 <- kimpo[kimpo$방향 == '하행' & kimpo$년도 == '2014',]
kimpo_down2015 <- kimpo[kimpo$방향 == '하행' & kimpo$년도 == '2015',]
kimpo_down2016 <- kimpo[kimpo$방향 == '하행' & kimpo$년도 == '2016',]

### *12종 차종 변수명 입력* : X1_X12
X1_X12 <- c("X1종", "X2종", "X3종", "X4종", "X5종", "X6종", "X7종", "X8종", "X9종", "X10종", "X11종", "X12종")

### *X1 제외 11종 차종 변수명 입력* : X2_X12
X2_X12 <- c("X2종", "X3종", "X4종", "X5종", "X6종", "X7종", "X8종", "X9종", "X10종", "X11종", "X12종")


### - 요약통계량 산출 (방향 기준, 1/2)
by(kimpo, kimpo$방향, summary)  # `방향` 기준

### - 요약통계량 산출 (방향 기준, 2/2)
#install.packages("Hmisc") #`Hmisc`패키지 설치 (※ 인스톨은 최초 1번만 실행)
library(Hmisc) #`Hmisc`패키지 로드
by(kimpo, kimpo$방향, describe) # `방향` 기준

### - 요약통계량 산출 (상행 x 연도별 기준)
by(kimpo_up, kimpo_up$년도, summary) # `상행` 기준

### - 요약통계량 산출 (하행 x 연도별 기준)
by(kimpo_down, kimpo_down$년도, summary) # `하행` 기준

### - 요약통계량 산출 (연도별 기준)
방향_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
for(loop_i in seq(1:length(방향_level))) {
  for(loop_j in seq(2012, 2016)) {
    data_name <- paste0(방향_level[loop_i], loop_j)
    print(data_name)
    data_name <- get(data_name)
    print(summary(data_name))
  }
}

### - 기술통계량 산출 (mean, median, var, sd, range, quantile, fivenum)
## 기술통계량 산출 (방향 x 차종 기준)
# 산술평균(mean)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, mean, na.rm = T))
# 중앙값(median)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, median, na.rm = T))
# 분산(var)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, var, na.rm = T))
# 표준편차(sd)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, sd, na.rm = T))
# 최대최소값(range)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, range, na.rm = T))
# 사분위수(quantile)
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, quantile, na.rm = T))
# fivenum
by(kimpo[X1_X12],kimpo$방향,function(x) sapply(x, fivenum, na.rm = T))

### 2-2) Box plot 산출
### - 시각화를 통한 데이터 탐색 준비
X1_X12 <- c("X1종", "X2종", "X3종", "X4종", "X5종", "X6종", "X7종", "X8종", "X9종", "X10종", "X11종", "X12종")
X2_X12 <- c("X2종", "X3종", "X4종", "X5종", "X6종", "X7종", "X8종", "X9종", "X10종", "X11종", "X12종")

### - Box plot을 통한 데이터 탐색 (방향 x 차종 기준) 
div <- par(mfrow = c(1,2))  # 가로 1줄, 세로 2줄로 화면을 분할함
boxplot(kimpo_up[X1_X12], main = "2012-2016 통합 상행 기준 (5종) Boxplot")
boxplot(kimpo_down[X1_X12], main = "2012-2016 통합 하행 기준 (5종) Boxplot")
par(div)

### - Box plot을 통한 데이터 탐색 (연도별 x 차종 기준) 
방향_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(2,5))  # 가로 2줄, 세로 5줄로 화면을 분할함
for (loop_i in seq(1:length(방향_level))) {
  for (loop_j in seq(2012, 2016)) {
    data <- get(paste0(방향_level[loop_i],loop_j))
    boxplot(data[X1_X12], main = paste0(방향_level[loop_i]," ",loop_j))
  }
}
par(div)


### 2-3) Kernel density plot 산출
### - Kernel density plot 산출 (상행 기준)
div <- par(mfrow = c(2,3)) # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(X1_X12))) {
  plot(density(kimpo_up[X1_X12[loop_i]][[1]], na.rm = T), main = paste0("2012-2016 상행 기준 ", X1_X12[loop_i]))
}
par(div) # 화면 분할을 종료함

### - Kernel density plot 산출 (하행 기준)
div <- par(mfrow = c(2,3)) # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(X1_X12))) {
  plot(density(kimpo_down[X1_X12[loop_i]][[1]], na.rm = T), main = paste0("2012-2016 하행 기준 ", X1_X12[loop_i]))
}
par(div) # 화면 분할을 종료함


### - Kernel density plot 산출 (상행 x 연도별 기준)
## 2012년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(X1_X12))) {
  plot(density(kimpo_up2012[X1_X12[loop_i]][[1]], na.rm = T), main = paste0("2012년 상행 ", X1_X12[loop_i]))
}
par(div)  # 화면 분할을 종료함


### 2-4) 상관관계와 산점도 산출
### - 상관관계와 산점도 산출 (상행 기준)
#install.packages("psych") #`psych`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(psych) #`psych`패키지 로드
psych::pairs.panels(kimpo_up[X1_X12],smooth = FALSE, ellipses = FALSE, main = "상행")

### - 상관관계와 산점도 산출 (상행 x 연도별 기준)
psych::pairs.panels(kimpo_up2012[X1_X12],smooth = FALSE, ellipses = FALSE, main = "2012년 상행")

#### 3. 분석을 위한 데이터 전처리
### 1) 변수명 변경 및 추출
### - 변수명 확인
colnames(kimpo)

### - 변수명 변경
colnames(kimpo) <- c('class','nameofroad','id','year','month','day','dayofweek','way','X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','total')
colnames(kimpo) #변경된 변수명을 확인

### - 필요한 변수만 추출
kimpo_tmp <- kimpo
kimpo_tmp$class <- NULL  # `class` 변수 삭제 
kimpo_tmp$id <- NULL  # `id` 변수 삭제
head(kimpo_tmp, 2)  # 변수 삭제 적용여부 테스트를 위해 kimpo 데이터셋 시작부터 2줄 출력
rm(kimpo_tmp) # 필요없는 데이터셋 삭제

### - 필요한 변수만 추출 (dplyr : select 사용)
#install.packages("dplyr") #`dplyr`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(dplyr) #`dplyr`패키지 로드
kimpo <- dplyr::select(kimpo, -(class), -(id))
head(kimpo,2)

### 2) 변수 생성
### - [연+월+일] 변수 생성
kimpo$month_adj <- ifelse(kimpo$month < 10, paste0("0",kimpo$month), kimpo$month)
kimpo$day_adj <- ifelse(kimpo$day < 10, paste0("0", kimpo$day), kimpo$day)
kimpo$date <- as.Date(paste0(kimpo$year, kimpo$month_adj, kimpo$day_adj), "%Y%m%d")
kimpo$month_adj <- NULL  # `month_adj` 변수 삭제
kimpo$day_adj <- NULL  # `day_adj` 변수 삭제
head(kimpo$date)
head(kimpo)

### - [연+월] 변수 생성
kimpo$yr_month <- format(as.Date(kimpo$date), "%Y-%m")
head(kimpo$yr_month,5)
head(kimpo)

### - [월+일] 변수 생성
kimpo$month_adj <- ifelse(kimpo$month < 10, paste0("0", kimpo$month), kimpo$month)
kimpo$day_adj <- ifelse(kimpo$day < 10, paste0("0", kimpo$day), kimpo$day)
kimpo$mdate <- paste0(kimpo$month_adj, kimpo$day_adj)
kimpo$month_adj <- NULL #"month_adj 변수 삭제
kimpo$day_adj <- NULL #"day_adj" 변수 삭제
head(kimpo$mdate, 5)
head(kimpo)

### - [주중, 주말] 변수 생성
kimpo$weekend <- ifelse(kimpo$dayofweek == "월", 0,
                        ifelse(kimpo$dayofweek == "화" ,0,
                               ifelse(kimpo$dayofweek == "수", 0,
                                      ifelse(kimpo$dayofweek == "목", 0,
                                             ifelse(kimpo$dayofweek =="금",0 ,
                                                    ifelse(kimpo$dayofweek == "토",1,1))))))
kimpo$weekend <- as.factor(kimpo$weekend)  # 변수 속성을 범주 속성으로 변경
summary(kimpo$weekend)

### - 휴일 정보 읽어오기 
holiday <- read.csv(file="./data/holiday.csv")
str(holiday)
holiday$date <- as.character(holiday$date)
holiday$date <- as.Date(holiday$date,"%Y%m%d")
holiday

### - [평일, 휴일, 공휴일] 변수 생성 
#install.packages("sqldf") #`sqldf`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(sqldf) #`sqldf`패키지 로드
kimpo <- sqldf::sqldf('
                      SELECT a.*, b.holiday
                      FROM kimpo a
                      LEFT JOIN holiday b
                      ON a.date = b.date
                      ')
kimpo$holiday <- ifelse(is.na(kimpo$holiday) & kimpo$weekend == 0,  0,
                        ifelse(is.na(kimpo$holiday) & kimpo$weekend == 1, 1, 2))  # 0 = 평일, 1 = 휴일 , 2 = 공휴일
head(kimpo,3)



### - [평일, 휴일 직전 평일, 휴일] 변수 생성
### 데이터셋을 방향 기준으로 분할
kimpo_up <- kimpo[kimpo$way == '상행',]  # 상행 구분을 분할함
kimpo_down <- kimpo[kimpo$way == '하행',]  # 하행 구분을 분할함

### 상행 데이터셋에 대한 변수 생성
kimpo_up$holiday1 <- dplyr::lead(kimpo_up$holiday, 1)  # holiday 컬럼 내 값들을 1개씩 위로 올려서 가져옴

kimpo_up$group_day[kimpo_up$holiday == 0] <- 0  # holiday 컬럼값이 0이라면 0 출력
kimpo_up$group_day[(kimpo_up$holiday == 0 & kimpo_up$holiday1 == 1) |
                     (kimpo_up$holiday == 0 & kimpo_up$holiday1 == 2)] <- 1 
# holiday 컬럼이 0이고 holiday1 컬럼이 0이거나, 또는 holiday 컬럼이 0이고 holiday1 컬럼이 2인 경우 1 출력
kimpo_up$group_day[kimpo_up$holiday == 1 | kimpo_up$holiday == 2] <- 2
# holiday 컬럼이 1이거나, 또는 holiday 컬럼이 2이면 2 출력

### 하행 데이터셋에 대한 변수 생성
kimpo_down$holiday1 <- dplyr::lead(kimpo_down$holiday, 1)  # holiday 컬럼 내 값들을 1개씩 위로 올려서 가져옴

kimpo_down$group_day[kimpo_down$holiday == 0] <- 0  # holiday 컬럼값이 0이라면 0 출력
kimpo_down$group_day[(kimpo_down$holiday == 0 & kimpo_down$holiday1 == 1) |
                       (kimpo_down$holiday == 0 & kimpo_down$holiday1 == 2)] <- 1 
# holiday 컬럼이 0이고 holiday1 컬럼이 0이거나, 또는 holiday 컬럼이 0이고 holiday1 컬럼이 2인 경우 1 출력
kimpo_down$group_day[kimpo_down$holiday == 1 | kimpo_down$holiday == 2] <- 2
# holiday 컬럼이 1이거나, 또는 holiday 컬럼이 2이면 2 출력

## 분할했던 데이터 합치기
kimpo <- rbind(kimpo_up, kimpo_down) # 상행/하행 구분을 다시 합침

kimpo$holiday1 <- NULL  # 불필요한 컬럼 삭제
rm(kimpo_up,kimpo_down)  # 불필요한 데이터셋 삭제

### - [차종] 관련 변수 생성 
kimpo$sedan <- kimpo$X1
kimpo$bus <- kimpo$X2
kimpo$s_cargo <- kimpo$X3 + kimpo$X4
kimpo$m_cargo <- kimpo$X5 + kimpo$X6 + kimpo$X7
kimpo$b_cargo <- kimpo$X8 + kimpo$X9 + kimpo$X10 + kimpo$X11 + kimpo$X12

head(kimpo, 3)


### 3) 변수 속성 변경 및 정렬
### - 변수 속성 바꾸기
kimpo$year <- as.factor(kimpo$year)
kimpo$month <- as.factor(kimpo$month)
kimpo$mdate <- as.factor(kimpo$mdate)
kimpo$holiday <- as.factor(kimpo$holiday)
kimpo$group_day<- as.factor(kimpo$group_day)
str(kimpo) #[kimpo] 데이터셋 변수 별 속성 확인

### - [일자] 기준 순서대로 정렬
#library(dplyr) #"dplyr"패키지 로드
kimpo <- dplyr::arrange(kimpo, way, date)  # way + date 기준으로 오름차순 정렬
head(kimpo,5)
tail(kimpo,5)

#### 4. 탐색적 데이터 분석 (일 구분)
### 1) 통계량 산출
### - 특정 데이터 영역 선택
kimpo_up <- kimpo[kimpo$way == '상행',]
kimpo_down <- kimpo[kimpo$way == '하행',]
kimpo_up2012 <- kimpo[kimpo$way == '상행' & kimpo$year == '2012',]
kimpo_up2013 <- kimpo[kimpo$way == '상행' & kimpo$year == '2013',]
kimpo_up2014 <- kimpo[kimpo$way == '상행' & kimpo$year == '2014',]
kimpo_up2015 <- kimpo[kimpo$way == '상행' & kimpo$year == '2015',]
kimpo_up2016 <- kimpo[kimpo$way == '상행' & kimpo$year == '2016',]
kimpo_down2012 <- kimpo[kimpo$way == '하행' & kimpo$year == '2012',]
kimpo_down2013 <- kimpo[kimpo$way == '하행' & kimpo$year == '2013',]
kimpo_down2014 <- kimpo[kimpo$way == '하행' & kimpo$year == '2014',]
kimpo_down2015 <- kimpo[kimpo$way == '하행' & kimpo$year == '2015',]
kimpo_down2016 <- kimpo[kimpo$way == '하행' & kimpo$year == '2016',]

### *5종 차종 변수명 입력 : xsedan_xcargo
# Cargo 제외 4종 차종 변수명 입력 : xbus_xcargo
xsedan_xcargo <- c('sedan','bus','s_cargo','m_cargo','b_cargo')
xbus_xcargo <- c('bus','s_cargo','m_cargo','b_cargo')

### - 요약통계량 산출 (방향 기준, 1/2)
by(kimpo, kimpo$way, summary)  # `방향` 기준

### - 요약통계량 산출 (방향 기준, 2/2)
#install.packages("Hmisc") #`Hmisc`패키지 설치 (※ 인스톨은 최초 1번만 실행)
library(Hmisc) #`Hmisc`패키지 로드
by(kimpo, kimpo$way, describe) # `방향` 기준

### - 요약통계량 산출 (상행 x 연도별 기준)
by(kimpo_up, kimpo_up$year, summary) # `상행` 기준

### - 요약통계량 산출 (하행 x 연도별 기준)
by(kimpo_down, kimpo_down$year, summary) # `하행` 기준

### - 요약통계량 산출 (연도별 기준)
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
for(loop_i in seq(1:length(way_level))) {
  for(loop_j in seq(2012, 2016)) {
    data_name <- paste0(way_level[loop_i], loop_j)
    print(data_name)
    data_name <- get(data_name)
    print(summary(data_name))
  }
}
rm(way_level)  # 불필요한 데이터셋을 삭제

### - 기술통계량 산출 (mean, median, var, sd, range, quantile, fivenum)
# 산술평균(mean)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, mean, na.rm = T))
# 중앙값(median)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, median, na.rm = T))
# 분산(var)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, var, na.rm = T))
# 표준편차(sd)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, sd, na.rm = T))
# 최대/최소값(range)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, range, na.rm = T))
# 사분위수(quantile)
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, quantile, na.rm = T))
# fivenum
by(kimpo[xsedan_xcargo],kimpo$way,function(x) sapply(x, fivenum, na.rm = T))

### - 기술통계량 산출 (상행 x 차종 x 일 구분 기준) 
# 산술평균(mean)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, mean, na.rm = T))
# 중앙값(median)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, median, na.rm = T))
# 분산(var)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, var, na.rm = T))
# 표준편차(sd)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, sd, na.rm = T))
# 최대/최소값(range)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, range, na.rm = T))
# 사분위수(quantile)
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, quantile, na.rm = T))
# fivenum
by(kimpo_up[xsedan_xcargo],kimpo_up$group_day,function(x) sapply(x, fivenum, na.rm = T))

### - 기술통계량 산출 (하행 x 차종 x 일 구분 기준) 
# 산술평균(mean)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, mean, na.rm = T))
# 중앙값(median)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, median, na.rm = T))
# 분산(var)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, var, na.rm = T))
# 표준편차(sd)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, sd, na.rm = T))
# 최대최소값(range)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, range, na.rm = T))
# 사분위수(quantile)
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, quantile, na.rm = T))
# fivenum
by(kimpo_down[xsedan_xcargo],kimpo_down$group_day,function(x) sapply(x, fivenum, na.rm = T))

### - 기술통계량 산출 (연도별 x 차종 x 일 구분 기준)
## 산술평균(mean)
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, mean, na.rm = T)))
  }
} 
## 중앙값(median)
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, median, na.rm = T)))
  }
} 
## 분산(var)
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, var, na.rm = T)))
  }
} 
## 표준편차(sd)
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, sd, na.rm = T)))
  }
} 
## 최대/최소값(range)
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, range, na.rm = T)))
  }
} 
## 사분위수(quantile)
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, quantile, na.rm = T)))
  }
} 
## fivenum
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data<- paste0(way_level[loop_i],loop_j)
    print(data)
    data <- get(data)
    print(by(data[xsedan_xcargo], data$group_day, function(x) sapply(x, fivenum, na.rm = T)))
  }
} 
rm(way_level)  # 불필요한 데이터셋을 삭제


### 2) Box plot 산출
### - Box plot을 통한 데이터 탐색 (방향 x 차종 기준) 
div <- par(mfrow = c(1,2))  # 가로 1줄, 세로 2줄로 화면을 분할함
boxplot(kimpo_up[xsedan_xcargo], main = "2012-2016 통합 상행 기준 (5종) Boxplot")
boxplot(kimpo_down[xsedan_xcargo], main = "2012-2016 통합 하행 기준 (5종) Boxplot")
par(div) # 화면 분할을 종료함

### - Box plot을 통한 데이터 탐색 (연도별 x 차종 기준) 
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(2,5))  # 가로 2줄, 세로 5줄로 화면을 분할함
for (loop_i in seq(1:length(way_level))) {
  for (loop_j in seq(2012, 2016)) {
    data <- get(paste0(way_level[loop_i],loop_j))
    boxplot(data[xsedan_xcargo], main = paste0(way_level[loop_i]," ",loop_j))
  }
}
par(div) # 화면 분할을 종료함
rm(way_level)  # 불필요한 데이터셋을 삭제

### 3) 시계열 추세 확인
### - 일간 시계열 추세 확인 (상행 x 차종 x 일 기준)
div <- par(mfrow = c(2,3))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up$date, kimpo_up[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2012-2016 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up$date, format= "%Y%m%d", labels = T)
}
par(div)

### - 일간 시계열 추세 확인 (하행 x 차종 x 일 기준)
div <- par(mfrow = c(2,3))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down$date, kimpo_down[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2012-2016 하행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down$date, format= "%Y%m%d", labels = T)
}
par(div)


### - 일간 시계열 추세 확인 (상행 x 연도별 x 차종 x 일 구분 기준)
## 2012년 상행
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up2012$date, kimpo_up2012[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2012 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up2012$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2013년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up2013$date, kimpo_up2013[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2013 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up2013$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2014년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up2014$date, kimpo_up2014[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2014 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up2014$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함


## 2015년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up2015$date, kimpo_up2015[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2015 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up2015$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2016년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_up2016$date, kimpo_up2016[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2016 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_up2016$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함
rm(way_level)  # 불필요한 데이터셋을 삭제


### - 일간 시계열 추세 확인 (하행 x 연도별 x 차종 x 일 구분 기준)
## 2012년 하행
way_level <- c('kimpo_down', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down2012$date, kimpo_down2012[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2012 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down2012$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2013년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down2013$date, kimpo_down2013[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2013 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down2013$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2014년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down2014$date, kimpo_down2014[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2014 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down2014$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2015년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down2015$date, kimpo_down2015[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2015 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down2015$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함

## 2016년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  plot(kimpo_down2016$date, kimpo_down2016[xsedan_xcargo[loop_i]][[1]], type='l', main = paste0("2016 상행 기준 (5종) ", xsedan_xcargo[loop_i]))
  axis.Date(side=1, at=kimpo_down2016$date, format= "%m%d", labels = T)
}
par(div)  # 화면 분할을 종료함
rm(way_level)  # 불필요한 데이터셋을 삭제


### - 월간 시계열 추세 확인 (상행/하행 x 차종 x 월 평균 기준)
## arregrate 함수를 이용하여 일별 데이터를 월별로 평균 계산 데이터셋 생성
# 상행
table_up <- aggregate(kimpo_up[xsedan_xcargo], by = list(kimpo_up$month), mean, na.rm = T)
summary(table_up)
# 하행
table_down <- aggregate(kimpo_down[xsedan_xcargo], by = list(kimpo_down$month), mean, na.rm = T)
summary(table_down)
# 월간 시계열 추세 확인 (상행 x 차종 x 월 평균 기준)  - BarPlot
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up[xsedan_xcargo[loop_i]][[1]], main = paste0("2012-2016 상행 기준 (월평균) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up$Group.1))
}
par(div)
# 월간 시계열 추세 확인 (하행 x 차종 x 월 평균 기준)  - BarPlot
way_level <- c('kimpo_up', 'kimpo_down') # 데이터셋의 이름을 저장
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down[xsedan_xcargo[loop_i]][[1]], main = paste0("2012-2016 하행 기준 (월평균) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down$Group.1))
}
par(div)
rm(table_up, table_down)

### - 월간 시계열 추세 확인 (상행 x 연도별 x 차종 x 월 평균 기준)
## arregrate 함수를 이용하여 일별 데이터를 월별로 평균 계산 데이터셋 생성
table_up_2012 <- aggregate(kimpo_up2012[xsedan_xcargo], by = list(kimpo_up2012$month), mean, na.rm = T)
table_up_2013 <- aggregate(kimpo_up2013[xsedan_xcargo], by = list(kimpo_up2013$month), mean, na.rm = T)
table_up_2014 <- aggregate(kimpo_up2014[xsedan_xcargo], by = list(kimpo_up2014$month), mean, na.rm = T)
table_up_2015 <- aggregate(kimpo_up2015[xsedan_xcargo], by = list(kimpo_up2015$month), mean, na.rm = T)
table_up_2016 <- aggregate(kimpo_up2016[xsedan_xcargo], by = list(kimpo_up2016$month), mean, na.rm = T)
## 2012년 상행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up_2012[xsedan_xcargo[loop_i]][[1]], main = paste0("2012 상행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up_2012$Group.1))
}
par(div)
## 2013년 상행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up_2013[xsedan_xcargo[loop_i]][[1]], main = paste0("2013 상행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up_2013$Group.1))
}
par(div)
## 2014년 상행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up_2014[xsedan_xcargo[loop_i]][[1]], main = paste0("2014 상행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up_2014$Group.1))
}
par(div)
## 2015년 상행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up_2015[xsedan_xcargo[loop_i]][[1]], main = paste0("2015 상행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up_2015$Group.1))
}
par(div)
## 2016년 상행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_up_2016[xsedan_xcargo[loop_i]][[1]], main = paste0("2016 상행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_up_2016$Group.1))
}
par(div)
rm(table_up_2012, table_up_2013, table_up_2014, table_up_2015, table_up_2016)

### - 월간 시계열 추세 확인 (하행 x 연도별 x 차종 x 월 평균 기준)
## arregrate 함수를 이용하여 일별 데이터를 월별로 평균 계산 데이터셋 생성
table_down_2012 <- aggregate(kimpo_down2012[xsedan_xcargo], by = list(kimpo_down2012$month), mean, na.rm = T)
table_down_2013 <- aggregate(kimpo_down2013[xsedan_xcargo], by = list(kimpo_down2013$month), mean, na.rm = T)
table_down_2014 <- aggregate(kimpo_down2014[xsedan_xcargo], by = list(kimpo_down2014$month), mean, na.rm = T)
table_down_2015 <- aggregate(kimpo_down2015[xsedan_xcargo], by = list(kimpo_down2015$month), mean, na.rm = T)
table_down_2016 <- aggregate(kimpo_down2016[xsedan_xcargo], by = list(kimpo_down2016$month), mean, na.rm = T)
## 2012년 하행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down_2012[xsedan_xcargo[loop_i]][[1]], main = paste0("2012 하행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down_2012$Group.1))
}
par(div)
## 2013년 하행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down_2013[xsedan_xcargo[loop_i]][[1]], main = paste0("2013 하행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down_2013$Group.1))
}
par(div)
## 2014년 하행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down_2014[xsedan_xcargo[loop_i]][[1]], main = paste0("2014 하행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down_2014$Group.1))
}
par(div)
## 2015년 하행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down_2015[xsedan_xcargo[loop_i]][[1]], main = paste0("2015 하행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down_2015$Group.1))
}
par(div)
## 2016년 하행 월 기준 (산술평균)
div <- par(mfrow = c(3,2))
for (loop_i in seq(1 : length(xsedan_xcargo))) {
  barplot(table_down_2016[xsedan_xcargo[loop_i]][[1]], main = paste0("2016 하행 기준 월평균(5종) ", xsedan_xcargo[loop_i]),
          names.arg = c(table_down_2016$Group.1))
}
par(div)
rm(table_down_2012, table_down_2013, table_down_2014, table_down_2015, table_down_2016)

### 4) Kernel density plot 산출
### - Kernel density plot 산출 (상행 기준)
div <- par(mfrow = c(2,3))
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2012-2016 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)

### - Kernel density plot 산출 (하행 기준)
div <- par(mfrow = c(2,3))
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2012-2016 하행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)

### - Kernel density plot 산출 (상행 x 연도별 기준)
## 2012년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up2012[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2012 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함
## 2013년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up2013[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2013 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함
## 2014년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up2014[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2014 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함
## 2015년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up2015[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2015 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함
## 2016년 상행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_up2016[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2016 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

### - Kernel density plot 산출 (하행 x 연도별 기준)
## 2012년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down2012[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2012 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

## 2013년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down2013[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2013 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

## 2014년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down2014[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2014 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

## 2015년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down2015[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2015 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

## 2016년 하행
div <- par(mfrow = c(2,3))  # 가로 2줄, 세로 3줄로 화면을 분할함
for (loop_i in seq(length(xsedan_xcargo))) {
  plot(density(kimpo_down2016[xsedan_xcargo[loop_i]][[1]], na.rm = T), main = paste0("2016 상행 기준 ", xsedan_xcargo[loop_i]))
}
par(div)  # 화면 분할을 종료함

### 5) 상관관계와 산점도 산출
### - 상관관계와 산점도 산출 (상행 기준)
psych::pairs.panels(kimpo_up[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "상행")

### - 상관관계와 산점도 산출 (하행 기준)
psych::pairs.panels(kimpo_down[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "하행")

### - 상관관계와 산점도 산출 (상행 x 연도별 기준)
## 2012년 상행
psych::pairs.panels(kimpo_up2012[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2012년 상행")
## 2013년 상행
psych::pairs.panels(kimpo_up2013[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2013년 상행")
## 2014년 상행
psych::pairs.panels(kimpo_up2014[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2014년 상행")
## 2015년 상행
psych::pairs.panels(kimpo_up2015[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2015년 상행")
## 2016년 상행
psych::pairs.panels(kimpo_up2016[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2016년 상행")

### - 상관관계와 산점도 산출 (하행 x 연도별 기준)
## 2012년 하행
psych::pairs.panels(kimpo_down2012[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2012년 하행")
## 2013년 하행
psych::pairs.panels(kimpo_down2013[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2013년 하행")
## 2014년 하행
psych::pairs.panels(kimpo_down2014[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2014년 하행")
## 2015년 하행
psych::pairs.panels(kimpo_down2015[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2015년 하행")
## 2016년 하행
psych::pairs.panels(kimpo_down2016[xsedan_xcargo],smooth = FALSE, ellipses = FALSE, main = "2016년 하행")


#### 5. 교통량을 사용한 예측 모델링
### 1) 예측모델링을 위한 데이터 전처리
### - [연+월] 기준 산술평균 및 빈도 데이터셋 생성
#library(sqldf)
up_month_total <- sqldf::sqldf(' 
                               SELECT kimpo_up.year, kimpo_up.yr_month, 
                               avg(kimpo_up.sedan) as mean_x, count(kimpo_up.sedan) as count_x
                               FROM kimpo_up
                               GROUP BY kimpo_up.yr_month
                               ')
up_month_total

### - [연+월], [평일, 휴일 직전 평일, 휴일] 기준 산술평균 및 빈도 데이터셋 생성
#library(sqldf)
up_month <- sqldf::sqldf(' 
                         SELECT kimpo_up.year, kimpo_up.yr_month, kimpo_up.group_day, 
                         avg(kimpo_up.sedan) as mean_x, count(kimpo_up.sedan) as count_x
                         FROM kimpo_up
                         GROUP BY kimpo_up.yr_month, kimpo_up.group_day
                         ')
up_month

### - 월 구분 변수 생성
up_month$month <- substr(up_month$yr_month,6,7)  # yr_month 변수의 6번째에서 7번째 문자열을 추출 
up_month_total$month <- substr(up_month_total$yr_month,6,7)  # yr_month 변수의 6번째에서 7번째 문자열을 추출
up_month_total$group_day <- 3  # 전체값에 3을 부여
up_month_total$group_day <- as.factor(up_month_total$group_day)  # 추가한 변수의 속성을 범주형으로 변경
agg.up.month <- rbind(up_month, up_month_total)  # `전체[3]`을 추가한 케이스를 행 기준으로 결합

### - 데이터 정렬
agg.up.month <- agg.up.month[with(agg.up.month, order(agg.up.month$yr_month, agg.up.month$group_day)),]


### - 분석 데이터셋(48개 케이스) 생성 >> 13개 row(12개 독립변인 + 1개 종속변인)로 구성된 데이터프레임 생성
order_reg <- c(seq(1:13))
raw_normal_day <- data.frame(order_reg)
raw_normal_day
### - 분석 데이터셋(48개 케이스) 생성 >> [평일]만 추출하여 별도 데이터셋 생성
total_normal_day <- agg.up.month[agg.up.month$group_day == 0,]  # `agg.up.month` 에서 평일만 가져옴
head(total_normal_day, 3)
### - 분석 데이터셋(48개 케이스) 생성 >>  데이터셋 내용 입력
lastrow <- nrow(total_normal_day)-12  ## 전체 60개월(5년 x 12개월) 중 1년치(12개월) 제외
for (loop_i in 1:lastrow) {
  loop_j <- loop_i + 12
  ## 12기간 데이터만 추출 (주의 : 월 기준 정렬 필수)
  temp <- total_normal_day[loop_i:loop_j,]$mean_x
  ## cbind를 이용하여 데이터 합치기
  raw_normal_day <- cbind(raw_normal_day,temp)
  ## 변수명 변환 
  changename <- loop_i
  names(raw_normal_day)[names(raw_normal_day) == "temp"] <- changename
}
raw_normal_day[,1:6]  # 6행만 출력

### - 분석 데이터셋(48개 케이스) 생성 >>  데이터셋 직각 회전
t_raw_normal_day <- as.data.frame(t(raw_normal_day))
head(t_raw_normal_day, 6)


### - 분석 데이터셋(48개 케이스) 생성 >>  불필요한 컬럼 제거
t_raw_normal_day <- as.data.frame(t_raw_normal_day[-1, ])
head(t_raw_normal_day, 6)

### - 분석 데이터셋(48개 케이스) 생성 >>  컬럼명 변경
colnames(t_raw_normal_day) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "Y")
colnames(t_raw_normal_day)

### - training 기간(Y값 기준 2013-2014년) 데이터만 추출
t_training_normal_day <- subset(t_raw_normal_day, as.numeric(rownames(t_raw_normal_day)) < 25)
t_training_normal_day



### 2) 탐색적 데이터 분석 (월 단위)
### - 연도별 x [평일, 휴일 직전 평일, 휴일] 기준 시계열 추세 그래프
library(ggplot2)
ggplot(data=agg.up.month, aes(x=month, y=mean_x, colour=year, group = year)) +
  geom_line() + facet_grid(group_day ~ year)

### - 연도별 x [평일, 휴일 직전 평일, 휴일] 기준 Boxplot
library(ggplot2)
ggplot(data=agg.up.month, aes(x=year, y=mean_x, fill=year, group = year)) +
  geom_boxplot() + facet_grid( ~ group_day) +
  scale_fill_brewer(palette="Blues")


### 3) ARIMA 예측모델
### - 교통량을 시계열 형태로 변환
# total_normal_day : [평일, 휴일 직전 평일, 휴일] 데이터셋에서 [평일] 데이터만 추출한 데이터셋
total_normal_day.training <- total_normal_day[total_normal_day$year == '2012' | total_normal_day$year == '2013' | total_normal_day$year == '2014', ]
ts_tr_week <- ts(data = total_normal_day.training$mean_x, start=c(2012,1), frequency = 12)
ts_tr_week

### - 시계열 요소 분해 (plot 함수)
plot(stl(ts_tr_week, s.window = "periodic"))

### - 단위근 검정(unit root test)
#install.packages("tseries") #`tseries`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(tseries)
tseries::adf.test(ts_tr_week, alternative = "stationary")
tseries::adf.test(ts_tr_week, alternative = "stationary", k=0)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=1)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=2)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=3)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=4)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=5)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=6)
tseries::adf.test(ts_tr_week, alternative = "stationary", k=7)

### - 비선형 검정 (Keenan 검정)
#install.packages("TSA") #`TSA`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(TSA)
TSA::Keenan.test(ts_tr_week)
TSA::Keenan.test(ts_tr_week, order=1)
TSA::Keenan.test(ts_tr_week, order=2)
TSA::Keenan.test(ts_tr_week, order=3)
TSA::Keenan.test(ts_tr_week, order=4)
TSA::Keenan.test(log(ts_tr_week))

### - 이분산성 검정 (McLeod.Li 검정)
#install.packages("TSA") #`TSA`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(TSA)
TSA::McLeod.Li.test(y=ts_tr_week)

### - ARIMA 모델 생성
#install.packages("forecast") #`forecast`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(forecast)
forecast::auto.arima(ts_tr_week)
tsdiag(forecast::auto.arima(ts_tr_week))

### - ACF와 PACF 산출
div <- par(mfrow = c(2,1))
acf(ts_tr_week)
pacf(ts_tr_week)
par(div)

### - 과대적합진단
## 기존 ARIMA 모형
week200.arima <- arima(ts_tr_week, c(2,0,0), seasonal = list(order = c(0, 1, 0), period = 12))  
summary(week200.arima)    ##MAPE = 2.286564 AIC = 431.18
## p를 1 높인 경우
week300.arima <- arima(ts_tr_week, c(3,0,0), seasonal = list(order = c(0, 1, 0), period = 12)) 
summary(week300.arima)    ##MAPE = 2.250295 AIC = 433.02
## q를 1 높인 경우
week201.arima <- arima(ts_tr_week, c(2,0,1), seasonal = list(order = c(0, 1, 0), period = 12)) 
summary(week201.arima)    ##MAPE = 2.274301 AIC = 432.44


### - 잔차분석(Ljung - Box 검정)
Box.test(week200.arima$residuals, type = "Ljung-Box")
tsdiag(forecast::auto.arima(ts_tr_week))

### - 실제값과 예측값 비교 그래프 산출
## 2012-2016년 ARIMA 예측 모델 생성
week200.arima <- arima(ts_tr_week, c(2,0,0), seasonal = list(order = c(0, 1, 0), period = 12))
## 2012-2016년 ARIMA 예측값 산출
fitted.week200.arima <- fitted(week200.arima) # Training 데이터셋 시간대 예측
pred.week200.arima <- predict(week200.arima, n.ahead = 24) # 2015-2016년 예측

fitted.week200.arima.temp <- as.data.frame(as.numeric(fitted.week200.arima)) # 데이터 프레임 형식으로 변경
colnames(fitted.week200.arima.temp) <- "predicted"  # 컬럼명 변경

pred.week200.arima.temp <- as.data.frame(as.numeric(pred.week200.arima$pred))
colnames(pred.week200.arima.temp) <- "predicted"  # 컬럼명 변경

fitted.pred.week200.arima <- rbind(fitted.week200.arima.temp, pred.week200.arima.temp) # 열 결합

ts_week200.tr.arima <- ts(fitted.pred.week200.arima, start = 2012, frequency = 12) # 시계열 형식으로 환원
ts_week200.tr.arima

## 2012-2016년 실제 교통량 데이터를 시계열 형식으로 변환
ts_tr_week.total <- ts(data = total_normal_day$mean_x, start=c(2012,1), frequency = 12)
ts_tr_week.total

## 2012-2016년 ARIMA 예측 데이터와 실제 데이터 결합
#install.packages("xts") #`xts`패키지 설치 (※ 인스톨은 최초 1번만 실행)
library(xts)
plotraw.week200.tr.arima <- merge(as.xts(ts_tr_week.total), as.xts(ts_week200.tr.arima))
names(plotraw.week200.tr.arima)[1:2] <- c("actual", "predicted")
head(plotraw.week200.tr.arima, 3)

## actual과 predicted graph 같이 그리기
#install.packages("ggplot2") #`ggplot2`패키지 설치 (※ 인스톨은 최초 1번만 실행)
library(ggplot2)
ggplot(plotraw.week200.tr.arima, aes(x=as.POSIXct(index(plotraw.week200.tr.arima)))) + 
  geom_line(aes(y=predicted), col='red') + 
  geom_line(aes(y=actual), col='blue') + 
  theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="ARIMA(2,0,0)(0,1,0)[12]", x="Time", y="Observed / Fitted") + 
  theme(plot.title = element_text(size=18, face="bold"))

### - MSE / MAPE 검증
## validation period / test period
va.act.pred.arima <- plotraw.week200.tr.arima [c(37:48),]
ts.act.pred.arima <- plotraw.week200.tr.arima [c(49:60),]

## Validation 데이터 MSE / MAPE 산출
#install.packages("MLmetrics") #`MLmetrics`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(MLmetrics)
arima.week200.va.MSE <- MLmetrics::MSE(va.act.pred.arima$predicted,va.act.pred.arima$actual)
arima.week200.va.MAPE <- MLmetrics::MAPE(va.act.pred.arima$predicted,va.act.pred.arima$actual)
c(arima.week200.va.MSE, arima.week200.va.MAPE)

## Test 데이터 MSE / MAPE 산출
#install.packages("MLmetrics") #`MLmetrics`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(MLmetrics)
arima.week200.ts.MSE <- MLmetrics::MSE(ts.act.pred.arima$predicted,ts.act.pred.arima$actual)
arima.week200.ts.MAPE <- MLmetrics::MAPE(ts.act.pred.arima$predicted,ts.act.pred.arima$actual)
c(arima.week200.ts.MSE, arima.week200.ts.MAPE)

### 4) 중회귀분석 예측모델
### - 중회귀 모델 생성 (정교화 이전 최초 모델)
normal_day.lm = lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, data=t_training_normal_day)
summary(normal_day.lm)

### - 다중공선성 진단
#install.packages("car") #`car`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(car)
car::vif(normal_day.lm)


### - 다중공선성 진단 - 교차상관도표 작성
#install.packages("psych") #`psych`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(psych) #`psych`패키지 로드
psych::pairs.panels(t_training_normal_day, smooth = FALSE, ellipses = FALSE, main = "변수 간 상관관계")

### - 변수선택과정 >> 다중공선성 진단
car::vif(lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, data=t_training_normal_day))
car::vif(lm(Y ~ X1+X2+X3+X4+X6+X7+X8+X9+X10+X11+X12, data=t_training_normal_day))
car::vif(lm(Y ~ X1+X2+X3+X4+X6+X7+X8+X10+X11+X12, data=t_training_normal_day))
# X1과 X12는 Y의 상관계수가 높으므로 제거 대상에는 제외하고 진행 ( VIF <= 5가 될 때까지)
car::vif(lm(Y ~ X1+X3+X4+X6+X7+X8+X10+X11+X12, data=t_training_normal_day))
car::vif(lm(Y ~ X1+X3+X4+X7+X8+X10+X11+X12, data=t_training_normal_day))

### - 변수선택과정 >> 변수선택 기법 적용
## 다중공선성을 어느 정도 제거한 후에 변수선택 기법 적용
summary(lm(Y ~ X1+X3+X4+X7+X8+X10+X11+X12, data=t_training_normal_day))
week.test3.lm <- lm(Y ~ X1+X3+X4+X7+X8+X10+X11+X12, data=t_training_normal_day)
## 변수선택 기법 적용 - stepwise
step.result.lm <- step(week.test3.lm, direction="both")
summary(step.result.lm)
car::vif(step.result.lm)

## 변수선택 기법 적용 - forward   
forward.result.lm <- step(week.test3.lm, direction="forward")
summary(forward.result.lm)
car::vif(forward.result.lm)

## 변수선택 기법 적용 - backward  
backward.result.lm <- step(week.test3.lm, direction="backward")
summary(backward.result.lm)
car::vif(backward.result.lm)

### - 중회귀 모델 생성 (최종 모델)
week.test5.lm = lm(Y ~ X1+X4+X12, data=t_training_normal_day)
summary(week.test5.lm)
car::vif(week.test5.lm)

### - 예측 진행
predicted <- predict(week.test5.lm, t_raw_normal_day)
new.week.lm <- cbind(t_raw_normal_day, predicted)

colnames(new.week.lm)[13:14] <- c("actual", "predicted")
head(new.week.lm, 3)
new.week.lm

### - 실제값 데이터셋 결합 및 2012년 실제값 추가
plot.new.week.lm <- new.week.lm[,c("actual", "predicted")]  # [actual], [predicted] 변수 추출
plot.new.week.lm$yr_month <- as.Date(seq(as.Date('2013-1-1'),by='month',length=48))  # Date 형식으로 변경
# plot.new.week.lm$yr_month <- as.Date(seq(as.Date('2013-2-1'),by='month',length=47))  # Date 형식으로 변경

total_normal_day.temp2 <- total_normal_day[,c(2,4)]  # 실제값 데이터셋에서 [연도+월], [교통량] 변수 추출
colnames(total_normal_day.temp2) <- c("yr_month", "actual")  # 변수명 변경
total_normal_day.temp2$yr_month <- paste0(total_normal_day.temp2$yr_month, "-01")  # Data 형식으로 변경
total_normal_day.temp2$yr_month <- as.Date(total_normal_day.temp2$yr_month, "%Y-%m-%d")

plot.new.week.lm <- merge(total_normal_day.temp2, plot.new.week.lm, by = 'yr_month', all = TRUE)  # 실제값 데이터셋 병합
plot.new.week.lm <- plot.new.week.lm[,c(1,2,4)]  # [yr_month], [actual.x], [predicted] 변수 추출
names(plot.new.week.lm)[2] <- "actual"  # [actual.x] 변수 컬럼명 [actual]로 변경
plot.new.week.lm

### - 실제값과 예측값 비교 그래프 산출
library(ggplot2)
ggplot(plot.new.week.lm, aes(x=as.POSIXct(plot.new.week.lm$yr_month))) + 
  geom_line(aes(y=actual), col='blue') + 
  geom_line(aes(y=predicted), col='red') + 
  theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="중회귀 모형 (Y ~ X1+X4+X12)\n", x="Time", y="실제값(청색) / 예측값(적색)") + 
  theme(plot.title = element_text(size=18, face="bold"))

### - MSE / MAPE 검증
### validation period / test period
va.act.pred.lm <- plot.new.week.lm [c(37:48),]
ts.act.pred.lm <- plot.new.week.lm [c(49:60),]

## Error Measures 산출
## validation
#library(MLmetrics)
lm.va.MSE <- MLmetrics::MSE(va.act.pred.lm$predicted,va.act.pred.lm$actual)
lm.va.MAPE <- MLmetrics::MAPE(va.act.pred.lm$predicted,va.act.pred.lm$actual)
c(lm.va.MSE, lm.va.MAPE)

#library(MLmetrics)
lm.ts.MSE <- MLmetrics::MSE(ts.act.pred.lm$predicted,ts.act.pred.lm$actual)
lm.ts.MAPE <- MLmetrics::MAPE(ts.act.pred.lm$predicted,ts.act.pred.lm$actual)
c(lm.ts.MSE, lm.ts.MAPE)

### - 잔차 검정 - 등분산성 가정
plot(week.test5.lm$fitted.values, week.test5.lm$residuals, pch=19)

### - 잔차 검정 - 독립성 가정(DURBIN-WATSON)
#install.packages("lmtest") #`lmtest`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(lmtest)
lmtest::dwtest(week.test5.lm)

### - 잔차 검정 - 정규성 가정 (Shapiro-Wilk)
week.test5.lm.rstudent <- rstudent(week.test5.lm)
shapiro.test(week.test5.lm.rstudent)

### - 잔차 검정 - 정규성 가정 (ggplot)
qqplot(week.test5.lm$fitted.values, week.test5.lm$residuals)


### 5) SVM 예측모델
### - SVM 모델 생성
#install.packages("e1071") #`e1071`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(e1071)
week.svm <- e1071::svm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, data=t_training_normal_day, type = "eps-regression", kernel = "linear")
week.svm
## Cost, gamma값 탐색
tune.week.svm <- e1071::tune(e1071::svm, Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, data=t_training_normal_day, type = "eps-regression", ranges = list(cost = 2^(2:9)))
tune.week.svm$best.parameters # gamma, cost 값 조회
## `tune` 함수를 사용해서 함수가 제안한 cost 값을 적용하여 최종 모델을 생성합니다.
week.svm <- e1071::svm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12, cost = 8, data=t_training_normal_day)
week.svm

### - 예측 진행
pred.week.svm <- predict(week.svm, t_raw_normal_day)
new.week.svm <- cbind(t_raw_normal_day, pred.week.svm)

new.week.svm <- as.data.frame(new.week.svm)
colnames(new.week.svm)[13:14] <- c("actual", "predicted")

head(new.week.svm, 3)


### - 실제값 데이터셋 결합 및 2012년 실제값 추가
total_normal_day.temp2 <- total_normal_day[,c(2,4)]  # 실제값 데이터셋에서 [연도+월], [교통량] 변수 추출
colnames(total_normal_day.temp2) <- c("yr_month", "actual")  # 변수명 변경
total_normal_day.temp2$yr_month <- paste0(total_normal_day.temp2$yr_month, "-01")  # Data 형식으로 변경
total_normal_day.temp2$yr_month <- as.Date(total_normal_day.temp2$yr_month, "%Y-%m-%d")

new.week.svm <- new.week.svm[,13:14] # [actual], [predicted] 변수 추출
new.week.svm$yr_month <- as.Date(seq(as.Date('2013-1-1'),by='month',length=48))  # Data 형식으로 변경
# new.week.svm$yr_month <- as.Date(seq(as.Date('2013-2-1'),by='month',length=47))  # Data 형식으로 변경

plot.new.week.svm <- merge(total_normal_day.temp2, new.week.svm, by = 'yr_month', all = TRUE)  # 실제값 데이터셋 병합
plot.new.week.svm <- plot.new.week.svm[,c(1,2,4)]  # [yr_month], [actual.x], [predicted] 변수 추출
names(plot.new.week.svm)[2] <- "actual"  # [actual.x] 변수 컬럼명 [actual]로 변경
plot.new.week.svm


### - 실제값과 예측값 비교 그래프 산출
library(ggplot2)
ggplot(plot.new.week.svm, aes(x=as.POSIXct(plot.new.week.svm$yr_month))) + 
  geom_line(aes(y=actual), col='blue') + 
  geom_line(aes(y=predicted), col='red') + 
  theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="SVM\n", x="Time", y="실제값(청색) / 예측값(적색)") + 
  theme(plot.title = element_text(size=18, face="bold"))

### - MSE / MAPE 검증
## validation period / test period
va.act.pred.svm <- plot.new.week.svm [c(37:48),]
ts.act.pred.svm <- plot.new.week.svm [c(49:60),]

## Validation
#library(MLmetrics)
svm.va.MSE <- MLmetrics::MSE(va.act.pred.svm$predicted, va.act.pred.svm$actual)
svm.va.MAPE <- MLmetrics::MAPE(va.act.pred.svm$predicted, va.act.pred.svm$actual)
c(svm.va.MSE, svm.va.MAPE)

## Test
#library(MLmetrics)
svm.ts.MSE <- MLmetrics::MSE(ts.act.pred.svm$predicted, ts.act.pred.svm$actual)
svm.ts.MAPE <- MLmetrics::MAPE(ts.act.pred.svm$predicted, ts.act.pred.svm$actual)
c(svm.ts.MSE, svm.ts.MAPE)


### 6) 신경망 분석 예측모델
### - 입력 데이터 표준화 처리 (scaling)
# t_raw_normal_day 데이터셋으로부터 training / validation / test data 분리
t_tr_week.nn <- subset(t_raw_normal_day, as.numeric(rownames(t_raw_normal_day)) < 25)
t_va_week.nn <- subset(t_raw_normal_day, as.numeric(rownames(t_raw_normal_day)) > 24 & as.numeric(rownames(t_raw_normal_day)) < 37)
t_ts_week.nn <- subset(t_raw_normal_day, as.numeric(rownames(t_raw_normal_day)) > 36)


head(t_tr_week.nn, 2) # training 데이터셋 분리 확인
head(t_va_week.nn, 2) # validation 데이터셋 분리 확인
head(t_ts_week.nn, 2) # test 데이터셋 분리 확인


### - 조정변수 생성
## training 조정변수로 변환하기 
## 조정 변수 ---> 조정변수값 = (실제값 - 최소값) / (최대값 - 최소값)       
max1 <- apply(t_tr_week.nn, 2, max)  # `2`는 컬럼을 의미
min1 <- apply(t_tr_week.nn, 2, min)
t_tr_week.nn.adj <- as.data.frame(scale(t_tr_week.nn, center = min1, scale = max1 - min1))
head(t_tr_week.nn.adj)

## validation 조정변수로 변환하기 
## 조정 변수 ---> 조정변수값 = (실제값 - 최소값) / (최대값 - 최소값)       
max1 <- apply(t_va_week.nn, 2, max)  # `2`는 컬럼을 의미
min1 <- apply(t_va_week.nn, 2, min)
t_va_week.nn.adj <- as.data.frame(scale(t_va_week.nn, center = min1, scale = max1 - min1))
head(t_va_week.nn.adj)

## test 조정변수로 변환하기 
## 조정 변수 ---> 조정변수값 = (실제값 - 최소값) / (최대값 - 최소값)       
max1 <- apply(t_ts_week.nn, 2, max)  # `2`는 컬럼을 의미
min1 <- apply(t_ts_week.nn, 2, min)
t_ts_week.nn.adj <- as.data.frame(scale(t_ts_week.nn, center = min1, scale = max1 - min1))
head(t_ts_week.nn.adj)


### - 신경망 분석 분석 모델 생성
#install.packages("neuralnet") #`neuralnet`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(neuralnet)
set.seed(2018)
week.nn <- neuralnet::neuralnet(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12, data = t_tr_week.nn.adj,
                                hidden = c(3, 2), rep= 10, linear.output = T)

### - 신경망 구조 plot
plot(week.nn)

### - 신경망 모형 오브젝트 구조 정리
summary(week.nn)

### - 신경망 모형 계산 과정 출력
summary(week.nn)

### - 예측 진행
pred.tr.week.nn <- neuralnet::compute(week.nn, t_tr_week.nn.adj[,1:12]) # `training` 기간에 대한 예측
pred.va.week.nn <- neuralnet::compute(week.nn, t_va_week.nn.adj[,1:12]) # `validation` 기간에 대한 예측
pred.ts.week.nn <- neuralnet::compute(week.nn, t_ts_week.nn.adj[,1:12]) # `test` 기간에 대한 예측

### - 예측값을 조정 전 변수 기준으로 환원
pred.tr.week.nn1 <- pred.tr.week.nn$net.result*(max(t_tr_week.nn$Y)-min(t_tr_week.nn$Y))+min(t_tr_week.nn$Y)
pred.va.week.nn1 <- pred.va.week.nn$net.result*(max(t_va_week.nn$Y)-min(t_va_week.nn$Y))+min(t_va_week.nn$Y)
pred.ts.week.nn1 <- pred.ts.week.nn$net.result*(max(t_ts_week.nn$Y)-min(t_ts_week.nn$Y))+min(t_ts_week.nn$Y)

### - 예측값을 조정 전 데이터셋에 결합
t_tr_week.nn.adj.pred <- as.data.frame(cbind(t_tr_week.nn, pred.tr.week.nn1))
t_va_week.nn.adj.pred <- as.data.frame(cbind(t_va_week.nn, pred.va.week.nn1))
t_ts_week.nn.adj.pred <- as.data.frame(cbind(t_ts_week.nn, pred.ts.week.nn1))

head(t_tr_week.nn.adj.pred) # `training` 기간에 대한 예측 결과
head(t_va_week.nn.adj.pred) # `validation` 기간에 대한 예측 결과
head(t_ts_week.nn.adj.pred) # `test` 기간에 대한 예측 결과


### - 실제값과 예측값 비교 그래프 산출
## `training`, `validation`, `test` 데이터셋 하나로 결합
t_tr_week.nn.adj.pred.temp <- t_tr_week.nn.adj.pred[13:14]  #`training` 데이터셋 Y값, 예측값 추출
t_va_week.nn.adj.pred.temp <- t_va_week.nn.adj.pred[13:14]  #`validation` 데이터셋 Y값, 예측값 추출
t_ts_week.nn.adj.pred.temp <- t_ts_week.nn.adj.pred[13:14]  #`test` 데이터셋 Y값, 예측값 추출

colnames(t_tr_week.nn.adj.pred.temp) <- c("actual", "predicted")  #`training` 데이터셋 변수명 변경
colnames(t_va_week.nn.adj.pred.temp) <- c("actual", "predicted")  #`validation` 데이터셋 변수명 변경
colnames(t_ts_week.nn.adj.pred.temp) <- c("actual", "predicted")  #`test` 데이터셋 변수명 변경

# 예측 데이터셋 결합
plot.rawdata.nn <- rbind(t_tr_week.nn.adj.pred.temp, t_va_week.nn.adj.pred.temp, t_ts_week.nn.adj.pred.temp)
plot.rawdata.nn$yr_month <- as.Date(seq(as.Date('2013-1-1'),by='month',length=48))
plot.rawdata.nn

### - 실제값 데이터셋 결합 및 2012년 실제값 추가
total_normal_day.temp2 <- total_normal_day[,c(2,4)]  # 실제값 데이터셋에서 [연도+월], [교통량] 변수 추출
colnames(total_normal_day.temp2) <- c("yr_month", "actual")  # 변수명 변경
total_normal_day.temp2$yr_month <- paste0(total_normal_day.temp2$yr_month, "-01")  # Data 형식으로 변경
total_normal_day.temp2$yr_month <- as.Date(total_normal_day.temp2$yr_month, "%Y-%m-%d")

plot.rawdata.nn <- merge(total_normal_day.temp2, plot.rawdata.nn, by = 'yr_month', all = TRUE)  # 실제값 데이터셋 병합
# plot.rawdata.nn <- merge(total_normal_day.temp2, plot.rawdata.nn, by = 'actual', all = TRUE)  # 실제값 데이터셋 병합
plot.rawdata.nn <- plot.rawdata.nn[,c(1,2,4)]  # [yr_month], [actual.x], [predicted] 변수 추출
names(plot.rawdata.nn)[2] <- "actual"  # [actual.x] 변수 컬럼명 [actual]로 변경
plot.rawdata.nn

### - 실제값과 예측값 비교 그래프 산출
library(ggplot2)
ggplot(plot.rawdata.nn, aes(x=as.POSIXct(plot.rawdata.nn$yr_month))) + 
  geom_line(aes(y=actual), col='blue') + 
  geom_line(aes(y=predicted), col='red') + 
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="Neural Network\n", x="Time", y="실제값(청색) / 예측값(적색)") + 
  theme(plot.title = element_text(size=18, face="bold"))

### - MSE / MAPE 검증
## validation period / test period
va.act.pred.nn <- plot.rawdata.nn [c(37:48),]
ts.act.pred.nn <- plot.rawdata.nn [c(49:60),]

## validation
#library(MLmetrics)
nn.va.MSE <- MLmetrics::MSE(va.act.pred.nn$predicted,va.act.pred.nn$actual)
nn.va.MAPE <- MLmetrics::MAPE(va.act.pred.nn$predicted,va.act.pred.nn$actual)
c(nn.va.MSE, nn.va.MAPE)

## Test
#library(MLmetrics)
nn.ts.MSE <- MLmetrics::MSE(ts.act.pred.nn$predicted,ts.act.pred.nn$actual)
nn.ts.MAPE <- MLmetrics::MAPE(ts.act.pred.nn$predicted,ts.act.pred.nn$actual)
c(nn.ts.MSE, nn.ts.MAPE)


### 7) Holt-Winters 예측모델
### - 모델 생성
## 초기 모델 생성(계수 미지정)
week.holt <- HoltWinters(ts_tr_week)
# ts_tr_week : [평일, 휴일 직전 평일, 휴일] 데이터셋에서 [평일] 데이터만 추출한 Training 데이터셋

## 모델 생성(계수 지정)
week.holt <- HoltWinters(ts_tr_week, alpha = 0.2, beta = 0.001, gamma = 0.3, seasonal = c("multiplicative"))

### - 예측 진행
## 2012-2016년 Holtwinters 예측 모델 생성
week.holt <- HoltWinters(ts_tr_week, alpha = 0.2, beta = 0.001, gamma = 0.3, seasonal = c("multiplicative"))

### - 실제값과 예측값 비교 그래프 산출
## 2012-2016년 HoltWinters 예측값 산출 
# install.packages("forecast")
library(forecast)
week.holt <- HoltWinters(ts_tr_week, alpha = 0.2, beta = 0.001, gamma = 0.3, seasonal = c("multiplicative"))

fitted.week.holt <- as.numeric(week.holt$fitted[, 1]) # Training 데이터셋 시간대 예측(2013-2014)
pred.week.holt <- forecast::forecast(week.holt, h=24) # 2015-2016년 예측
pred.week.holt <- as.data.frame(pred.week.holt)
pred.week.holt <- pred.week.holt[,1] # 예측값이 들이었는 변수만 추 출

fitted.week.holt.temp <- as.data.frame(as.numeric(fitted.week.holt)) # 데이터 프레임 형식으로 변경
colnames(fitted.week.holt.temp) <- "predicted"  # 컬럼명 변경

pred.week.holt.temp <- as.data.frame(as.numeric(pred.week.holt)) # 데이터 프레임 형식으로 변경
colnames(pred.week.holt.temp) <- "predicted"  # 컬럼명 변경

ts_pred <- ts(rbind(fitted.week.holt.temp, pred.week.holt.temp), start = 2013, frequency = 12)
# 열 결합 + 시계열 형식으로 환원
ts_pred

## 2013-2016년 실제 교통량 데이터를 시계열 형식으로 변환
ts_tt_week <- ts(data = total_normal_day$mean_x, start=c(2012,1), frequency = 12)
ts_tt_week

## 2013-2016년 Holtwinters 예측 데이터와 실제 데이터 결합
library(xts)
plotraw.tt.holt <- merge(as.xts(ts_tt_week), as.xts(ts_pred))
names(plotraw.tt.holt)[1:2] <- c("actual", "predicted")
tail(plotraw.tt.holt, 4)

### - 실제값과 예측값 비교 그래프 산출
## actual과 predicted graph 같이 그리기
library(ggplot2)
ggplot(plotraw.tt.holt, aes(x=as.POSIXct(index(plotraw.tt.holt)))) + 
  geom_line(aes(y=predicted), col='red') + 
  geom_line(aes(y=actual), col='blue') + 
  theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="Holt-Winters filtering\n", x="Time", y="Observed / Fitted") + 
  theme(plot.title = element_text(size=18, face="bold"))

### - MSE / MAPE 검증
## validation period / test period
va.act.pred.holt <- plotraw.tt.holt [c(37:48),]
ts.act.pred.holt <- plotraw.tt.holt [c(49:60),]

## validation
#library(MLmetrics)
holt.va.MSE <- MLmetrics::MSE(va.act.pred.holt$predicted,va.act.pred.holt$actual)
holt.va.MAPE <- MLmetrics::MAPE(va.act.pred.holt$predicted,va.act.pred.holt$actual)
c(holt.va.MSE, holt.va.MAPE)

## test
#library(MLmetrics)
holt.ts.MSE <- MLmetrics::MSE(ts.act.pred.holt$predicted,ts.act.pred.holt$actual)
holt.ts.MAPE <- MLmetrics::MAPE(ts.act.pred.holt$predicted,ts.act.pred.holt$actual)
c(holt.ts.MSE, holt.ts.MAPE)


### 8) 모형 간 비교 검증
### - plot 산출을 위한 데이터 전처리
yr_month <- as.Date(seq(as.Date('2012-1-1'),by='month',length=60))
final.data <-as.data.frame(yr_month)
final.data$actual <- as.numeric(plotraw.week200.tr.arima[,1]) # 실제값 입력

## 각 모델별 예측치를 merge 하기 위해서 각 데이터 이름을 먼저 배열로 정리
data.bymodel <- c('plotraw.week200.tr.arima', 'plot.new.week.lm', 'plot.new.week.svm', 'plot.rawdata.nn', 'plotraw.tt.holt')

## 각 모델별 dataset 에서 예측치만 가져옴
for (loop_i in seq(length(data.bymodel))) {
  extract.predicted <- get(data.bymodel[loop_i])
  extract.predicted <- extract.predicted[,"predicted"]
  final.data <- cbind(final.data, as.numeric(extract.predicted))
}

## 칼럼 이름 변경
names(final.data) <- c('yr_month', 'ACTUAL', 'ARIMA_M', 'REG_M', 'SVM_M', 'NN_M', 'HOLTWINTERS_M')

## melt 이용하여 data 형태 변환을 위해 data 형태 변환할 칼럼 이름 정의
select_variable10 <- c('ACTUAL', 'ARIMA_M', 'REG_M', 'SVM_M', 'NN_M', 'HOLTWINTERS_M')

## melt 함수를 이용하여 data 형태를 바꿔줌 
#install.packages("reshape2") #`reshape2`패키지 설치 (※ 인스톨은 최초 1번만 실행)
#library(reshape2)
final.data1 <- reshape2::melt(final.data, id.vars=colnames(final.data[1]), measure.vars = select_variable10, na.rm = FALSE)

## variable 속성은 factor 형태 ggplot 을 그리기 위해 character 형태로 변환
final.data1$variable <- as.character(final.data1$variable)


### - 각 모델 별 3X3 그래프 그리기
library(ggplot2)
ggplot(final.data1, aes(x=as.POSIXct(yr_month))) + 
  geom_line(aes(y=value, colour = variable)) + 
  #facet_grid(variable~.) +
  facet_wrap(~variable) +
  coord_cartesian(ylim = c(23000, 48000)) +
  ##theme_bw() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="FORCAST BY EACH MODEL\n", x="Time", y="Value") + 
  theme(plot.title = element_text(size=18, face="bold")) +
  theme(legend.position="none")


### - 각 모델을 한 차트에 그리기
library(ggplot2)
ggplot(final.data1, aes(x=as.POSIXct(yr_month), group =variable)) + 
  geom_line(aes(y=value, colour=variable, linetype=variable, size=variable )) +
  coord_cartesian(ylim = c(23000, 48000)) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-01-01")), 
             linetype="dashed") + 
  labs(title="FORCAST BY EACH MODEL\n", x="Time", y="Observed / Fitted") + 
  theme(plot.title = element_text(size=18, face="bold"), legend.position = "bottom") +
  ## scale_color_manual(values = c("black", "gray40", "gray40", "orange", "red", "gray40", "gray40", "gray40", "gray40")) +
  scale_color_manual(values = c("black", "green", "purple", "orange", "red", "yellow", "skyblue", "brown", "gray40")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid", "solid", "dashed", "dashed", "dashed", "dashed")) +
  scale_size_manual(values = c(1, 0.5, 0.5, 1, 1, 0.5, 0.5, 0.5, 0.5))


