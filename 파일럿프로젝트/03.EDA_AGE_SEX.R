# 데이터 로딩 ----
df_age_sex <- read.csv("./AGE_SEX/AGE_SEX_300.csv")

col_names <- c(
  "YYYYMM"
  ,"ID_300"
  ,"10_M"
  ,"20_M"
  ,"30_M"
  ,"40_M"
  ,"50_M"
  ,"60_M"
  ,"10_F"
  ,"20_F"
  ,"30_F"
  ,"40_F"
  ,"50_F"
  ,"60_F"
)

df_age_sex <- df_age_sex[,0:14] # 비율데이터 제거
colnames(df_age_sex) <- col_names # 변수명 변경
df_age_sex$YYYYMM <- as.factor(df_age_sex$YYYYMM) # YYYYMM을 명목함수로 변경

# 데이터 탐색 
head(df_age_sex)
str(df_age_sex)
summary(df_age_sex)

# 데이터 PIVOT 
# install.packages('reshape')
library(reshape)
df_age_sex_pivot <- melt(df_age_sex, id.vars = c("YYYYMM","ID_300")) # 데이터 PIVOT
                        
head(df_age_sex_pivot)
str(df_age_sex_pivot)
summary(df_age_sex_pivot)

library(sqldf)

df_age_sex_pivot <- sqldf("
  SELECT 
    YYYYMM,
    ID_300,
    substr(variable,1,2) as AGE,
    substr(variable,4,1) as SEX,
    value
  FROM df_age_sex_pivot 
") 

avg_by_age_sex <- sqldf("
  SELECT 
    YYYYMM,
    AGE, 
    SEX,
    AVG(value) as value 
  FROM df_age_sex_pivot 
  GROUP BY YYYYMM,AGE,SEX
") 

# 연령별 평균 유동인구
ggplot(avg_by_age_sex,aes(x=AGE,y=value)) + geom_bar(stat = "identity", fill = "dodgerblue4")

# 성별 평균 유동인구
ggplot(avg_by_age_sex,aes(x=SEX,y=value)) + geom_bar(stat = "identity", fill = "dodgerblue4")

# 연령/성별 평균 유동인구
ggplot(avg_by_age_sex,aes(x=AGE,y=value,fill=SEX))+geom_bar(stat = "identity")
ggplot(avg_by_age_sex,aes(x=AGE,y=value,fill=SEX))+geom_bar(stat = "identity",position=position_dodge())





