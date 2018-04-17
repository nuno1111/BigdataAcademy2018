#### PCA -> CLUSTERING ####
id_300_type <- read.csv("./DATA/ID_300_TYPE_PIVOT.csv")
id_300_type$X <- NULL

id_300_type_prcomp <- prcomp(id_300_type[,c(-1)],scale = T) # 주성분 분석

plot(id_300_type_prcomp, type="l", sub = "scree plot") # scree plot 
# 3번째 주성분에서 기울기가 급격히 변하므로 주성분 3개 선택

biplot(id_300_type_prcomp, cex = c(0.7,0.8))

# 관측치별 주성분 1, 주성분2 추출
pc1 <- predict(id_300_type_prcomp)[,1]
pc2 <- predict(id_300_type_prcomp)[,2]
plot(pc1,pc2)

# 유동인구를 넣어서도 한번 해보자!!
# 평균유동인구 가져오기
# df_time_pivot <- read.csv('./DATA/DF_TIME_PIVOT.csv')
# df_time_pivot <- df_time_pivot[df_time_pivot$YYYYMM != '201312',] # 2013/12 데이터 제거
# head(df_time_pivot)
# 
# library(sqldf)
# df_time_mean <- sqldf("SELECT ID_300, AVG(value) as mean_value FROM df_time_pivot GROUP BY ID_300 ")
# 
# id_300_type_with_pop <- merge(df_time_mean,id_300_type)

# PCA -> CLUSTERING
# 1. PCA
# id_300_type_with_pop_prcomp <- prcomp(id_300_type_with_pop[,c(-1)],scale = T) # 주성분 분석
# id_300_type_with_pop_prcomp
# 
# plot(id_300_type_with_pop_prcomp, type="l", sub = "scree plot") # scree plot 
# biplot(id_300_type_with_pop_prcomp, cex = c(0.7,0.8))

# 관측치별 주성분 1, 주성분2 추출
# pc1 <- predict(id_300_type_with_pop_prcomp)[,1]
# pc2 <- predict(id_300_type_with_pop_prcomp)[,2]
# plot(pc1,pc2)

# 관측치별 이름 매핑
# text(pc1, pc2, labels = id_300_type_with_pop$ID_300, cex = 0.5, pos = 1, col = "blue")

## 결론 : 유동인구 넣는다고 변별력이 없어서.. 안 넣는게 나을듯

# 2.군집분석
id_300_type_prc_df <-as.data.frame(cbind(pc1,pc2)) # 주성분분석 결과를 데이터 프레임으로 변환

id_300_type_kmeans <- kmeans(id_300_type_prc_df,centers = 8, iter.max = 1000) # k=5 인 kmeans 군집분석 수행

id_300_type_prc_df$cluster <- as.factor(id_300_type_kmeans$cluster) # 군집결과를 변수로 추가

# install.packages("caret")
library(caret)
qplot(pc1,pc2, colour = cluster, data = id_300_type_prc_df)





