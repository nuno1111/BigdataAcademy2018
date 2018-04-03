# 데이터 로딩 ----
df_time <- read.csv("./TIME/TIME_300.csv")

col_names <- c(
  "YYYYMM"
  ,"ID_300"
  ,"00"
  ,"01"
  ,"02"
  ,"03"
  ,"04"
  ,"05"
  ,"06"
  ,"07"
  ,"08"
  ,"09"
  ,"10"
  ,"11"
  ,"12"
  ,"13"
  ,"14"
  ,"15"
  ,"16"
  ,"17"
  ,"18"
  ,"19"
  ,"20"
  ,"21"
  ,"22"
  ,"23"
)

df_time <- df_time[,0:26] # 비율데이터 제거
colnames(df_time) <- col_names # 변수명 변경
df_time$YYYYMM <- as.factor(df_time$YYYYMM) # YYYYMM을 명목함수로 변경

# 데이터 탐색 
head(df_time)
str(df_time)
summary(df_time)

# 데이터 PIVOT 
# install.packages('reshape')
library(reshape)
df_time_pivot <- melt(df_time, id.vars = c("YYYYMM","ID_300")) # 데이터 PIVOT
                        
head(df_time_pivot)
str(df_time_pivot)
summary(df_time_pivot)

library(sqldf)
avg_by_time <- sqldf("SELECT YYYYMM,variable as Time, AVG(value) as value FROM df_time_pivot GROUP BY YYYYMM,variable") 

# 시간대별 평균 유동인구
ggplot(avg_by_time,aes(x=Time,y=value)) + geom_bar(stat = "identity", fill = "dodgerblue4")
ggplot(avg_by_time,aes(x=Time,y=value,fill=YYYYMM))+geom_bar(stat = "identity")

# 월별 평균 유동인구
ggplot(avg_by_time,aes(x=YYYYMM,y=value))+geom_bar(stat = "identity", fill = "dodgerblue4")
ggplot(avg_by_time,aes(x=YYYYMM,y=value,fill=Time))+geom_bar(stat = "identity")







