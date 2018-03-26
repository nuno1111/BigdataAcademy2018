# install.packages("pbapply")
# install.packages("corrplot")
# install.packages("psy")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("factoextra")
# install.packages("kohonen")
# install.packages("dbscan")
# install.packages("GPArotation")

#============================================================
# title: "요인분석"
# subtitle: "factor Analysis"
# author: "Begas"
# date: "2018.01.09"
#============================================================


#============================================================
# set directory path
#============================================================  

# ---- set directory path ---- 
# base_dir <- "C:\\Users\\user\\Desktop\\공유폴더\\실습자료\\3월22일"
# setwd(paste(base_dir, "Online Retail", sep = "\\"))
# setwd(base_dir)

#============================================================
# data import
#============================================================

# 데이터 불러오기
rawdata_df  <- read.csv("3월22일/data.csv", header = TRUE, stringsAsFactors = FALSE, sep = "\t")
head(rawdata_df)

#============================================================
# 데이터 탐색 
#============================================================

# 설문 문항만 가져오기 (grep 함수 활용)
data_df  <- rawdata_df[, names(rawdata_df)[grep("Q", names(rawdata_df))]]

# 데이터 결측 확인하기
sum(is.na(data_df))

# 항목 건수 확인
library(pbapply)
pbapply::pbapply(data_df, 2, table)

#============================================================
# 상관계수 행렬 
#============================================================

data_mat <- as.matrix(data_df) 
cor_mat  <- cor(data_mat)

library(corrplot)
x11(); corrplot::corrplot(cor_mat, type = "full", addCoefasPercent = TRUE, diag = TRUE, addCoef.col = TRUE)
x11(); corrplot::corrplot.mixed(cor_mat, main = "Example : Self esteem test")

#============================================================
# 설문 신뢰도 평가 
#============================================================

# psy package의 cronbach function
library(psy)
cat("- result : psy package  \n")
alpha_psy  <- psy::cronbach(data_mat)
alpha_psy
alpha_psy$alpha

# psych package의 alpha function
library(psych)
cat("- result : psych package  \n")
alpha_psych <- psych::alpha(data_mat)
alpha_psych
alpha_psych

# check.keys = TRUE 
cat(" ---------- Positive ---------- ")
alpha_psych <- psych::alpha(data_mat, check.keys = TRUE) # check.keys : 방향성을 보고 알아서 돌렸음
alpha_psych

# 분리된 문항만 선택
cat("\n \n \n ")
cat(" ----------  Negative ---------- ")
alpha_psych_negative <- psych::alpha(data_mat[,c(3, 5, 8, 9, 10)])
alpha_psych_negative

alpha_psych_positive <- psych::alpha(data_mat[,c(1, 2, 4, 6, 7)])
alpha_psych_positive



#============================================================
# 요인분석 적합성 확인 : Bartlett test of sphericity
#============================================================

#  Bartlett test of sphericity
bartlett_result <- psych::cortest.bartlett(cor_mat, nrow(data_mat))
bartlett_result

#============================================================
# 요인분석 적합성 확인 : KMO - 측도(Kaiser ??? Meyer ??? Olkin measure of sampling adequacy)
#============================================================

# KMO measure
KMO_result <- psych::KMO(cor_mat)
KMO_result


#============================================================
# 요인분석 : 요인 수 결정 
#============================================================

# 요인 수 결정 
# scree plot default
psych::VSS.scree(cor_mat, main = "scree plot")

# 누적분산비 기준 : eigenvalues를 이용하여 직접 plot 그리기
# eigenvalue
eigenvalues <- eigen(cor_mat)
e_values    <- eigenvalues$values

# eigenvalue dataframe
eigen_df <- data.frame(e_value = e_values, e_num = 1:length(e_values), stringsAsFactors = FALSE)

eigen_df$prop     <- e_values/sum(e_values)
eigen_df$cum_prop <- cumsum(e_values)/sum(e_values)
eigen_df


# plot : 누적 분산 비율
x11()
barplot(width = 1, height = eigen_df$cum_prop*100, col = "orange", names = paste0("e", 1:10), ylim = c(0,100), 
        xlab = "the number of factors",  ylab = "proportion of cumulative variance", main = "plot of prop- cumulative variance")

# plot : 개별 분산 비율 
x11()
barplot(width = 1, height = eigen_df$prop*100, col = "dodgerblue2", names = paste0("e", 1:10), ylim = c(0,100), 
        xlab = "the number of factors",  ylab = "proportion of variance", main = "plot of prop- variance")


# 참고 : use ggplot2 package : 누적 분산 plot
library(ggplot2)
x11()
ggplot2::ggplot(eigen_df, aes(x = e_num, y = cum_prop*100)) +
  geom_bar(fill = "orange", stat = "identity") + 
  abs(x = "the number of factors", y = "proportion of cumulative variance", title = "plot of prop- cumulative variance") +
  scale_x_discrete(limits = 1:10) + 
  coord_cartesian(ylim = c(0, 100)) + 
  theme_bw()  


# 참고 : use factoextra package : 개별 분산 scree plot
library(factoextra)
x11()
factoextra::fviz_eig(prcomp(data_mat, center = T, scale. = T), 
                     main = "Scree Plot(variance)", 
                     barcolor = "#00AFBB", 
                     barfill = "#00AFBB", 
                     linecolor = "#FC4E07", 
                     xlab = "the number of factors", 
                     ylab = "Proportion of Variance")


#============================================================
# 요인분석 : 모형적합 (2요인 모형, 3요인 모형) 
#============================================================
# 직교회전 
cat("\n ------------------------------------- orthogonal rotation ------------------------------------- \n")

# 모형적합 : ML
cat("\n ------------------------------------- ML algorithm ------------------------------------- \n")
cat("\n ----------- Fa 1 ----------- \n")
FA_1_result_ML <- psych::fa(cor_mat, nfactors = 1, n.obs = nrow(data_df), fm = "ml", rotate = "none")
FA_1_result_ML

cat("\n ----------- Fa 2 ----------- \n")
FA_2_result_ML <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "ml", rotate = "none")
FA_2_result_ML

cat("\n ----------- Fa 3 ----------- \n")
FA_3_result_ML <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "ml", rotate = "none")
FA_3_result_ML


# 요인적재값 
cat("\n ----------- loadings : Fa 1 ----------- \n")
FA_1_result_ML$loadings
cat("\n ----------- loadings : Fa 2 ----------- \n")
FA_2_result_ML$loadings
cat("\n ----------- loadings : Fa 3 ----------- \n")
FA_3_result_ML$loadings


# 모형적합 : 주축요인추출
cat("\n ------------------------------------- PA algorithm ------------------------------------- \n")
cat("\n ----------- Fa 2 ----------- \n")
FA_2_result_PA <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "pa", rotate = "none")
FA_2_result_PA

cat("\n ----------- Fa 3 ----------- \n")
FA_3_result_PA <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "pa", rotate = "none")
FA_3_result_PA

# 요인적재값 
cat("\n ----------- loadings : Fa 2 ----------- \n")
FA_2_result_PA$loadings
cat("\n ----------- loadings : Fa 3 ----------- \n")
FA_3_result_PA$loadings

# plot 
plot(FA_2_result_ML) ; cat(" \n FA_2_result_ML \n ")
plot(FA_3_result_ML) ; cat(" \n FA_3_result_ML \n ")
plot(FA_2_result_PA) ; cat(" \n FA_2_result_PA \n ")
plot(FA_3_result_PA) ; cat(" \n FA_3_result_PA \n ")

cat("\n ------------------------------------- fa function component ------------------------------------- \n")
names(FA_2_result_PA)

#============================================================
# 요인분석 : 요인 회전  
#============================================================
# 직교회전 
cat("\n ------------------------------------- orthogonal rotation ------------------------------------- \n")

# 모형적합 : ML
cat("\n ------------------------------------- ML algorithm orthogonal ------------------------------------- \n")
Varimax_2_result_ML <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "ml", rotate = "varimax")
Varimax_3_result_ML <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "ml", rotate = "varimax")

# 요인적재값 
Varimax_2_result_ML$loadings
Varimax_3_result_ML$loadings

# 로테이션 전후 비교
cbind(FA_2_result_ML$loadings,Varimax_2_result_ML$loadings )

# 모형적합 : 주축요인추출
cat("\n ------------------------------------- PA algorithm orthogonal ------------------------------------- \n")
Varimax_2_result_PA <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "pa", rotate = "varimax")
Varimax_3_result_PA <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "pa", rotate = "varimax")

# 요인적재값 
Varimax_2_result_PA$loadings
Varimax_3_result_PA$loadings

# 비직교회전 
cat("\n ------------------------------------- oblique rotation ------------------------------------- \n")

# 모형적합 : ML
cat("\n ------------------------------------- ML algorithm oblique ------------------------------------- \n")
oblimin_2_result_ML <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "ml", rotate = "oblimin")
oblimin_3_result_ML <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "ml", rotate = "oblimin")

# 요인적재값 
oblimin_2_result_ML$loadings
oblimin_3_result_ML$loadings

# 모형적합 : 주축요인추출
cat("\n ------------------------------------- PA algorithm oblique ------------------------------------- \n")
oblimin_2_result_PA <- psych::fa(cor_mat, nfactors = 2, n.obs = nrow(data_df), fm = "pa", rotate = "oblimin")
oblimin_3_result_PA <- psych::fa(cor_mat, nfactors = 3, n.obs = nrow(data_df), fm = "pa", rotate = "oblimin")

# 요인적재값 
oblimin_2_result_PA$loadings
oblimin_3_result_PA$loadings

# plot으로 확인하기
# orthogonal rotations
x11();
plot(Varimax_2_result_ML, cex = 1) ; cat(" \n Varimax_2_result_ML \n ")
plot(Varimax_3_result_ML, cex = 1.5) ; cat(" \n Varimax_3_result_ML \n ")
plot(Varimax_2_result_PA, cex = 1.5) ; cat(" \n Varimax_2_result_PA \n ")
plot(Varimax_3_result_PA, cex = 1.5) ; cat(" \n Varimax_3_result_PA \n ")

# oblique rotations
x11()
plot(oblimin_2_result_ML, cex = 1) ; cat(" \n oblimin_2_result_ML \n ")
plot(oblimin_3_result_ML, cex = 1.5) ; cat(" \n oblimin_3_result_ML \n ")
plot(oblimin_2_result_PA, cex = 1.5) ; cat(" \n oblimin_2_result_PA \n ")
plot(oblimin_3_result_PA, cex = 1.5) ; cat(" \n oblimin_3_result_PA \n ")

# 추가
tmp = factor.scores(data_df, Varimax_2_result_ML)
tmp
tmp$scores

#============================================================
# title: "판별분석"
# subtitle: "Discriminant Analysis"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 판별분석 예제
#============================================================

# SAheart: 남미의 심장병 발생 확률이 높은 지역의 남성에 대한 retrospective 조사를 한 데이터

library(MASS)

# data load
heart <- read.csv("3월22일/SAheart.csv")
str(heart)
dim(heart)
head(heart)
summary(heart)
# sbp: systolic blood pressure
# tobacco: cumulative tobacco (kg)
# ldl: low density lipoprotein cholesterol
# adiposity
# obesity
# alcohol: current alcohol consumption
# age
# chd: response, coronary heart disease

# 산점도 행렬
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(heart, upper.panel = panel.cor, diag.panel = panel.hist)

# 데이터 분할
train <- sample(1:nrow(heart), round(nrow(heart) * 0.7))
table(heart$chd[train])

# LDA
# train 자료에 대한 lda
lda.fit <- lda(factor(chd) ~ ., data = heart, subset = train)
# test 자료에 대한 예측
pred_chd.lda <- predict(lda.fit, newdata = heart[-train, ])
result.lda <- table(heart$chd[-train], pred_chd.lda$class)
result.lda
# 정분류율
accuracy.lda <- 100*sum(diag(result.lda))/sum(result.lda)
accuracy.lda

# QDA
# train 자료에 대한 qda
qda.fit <- qda(factor(chd) ~ ., data = heart, subset = train)
# test 자료에 대한 예측
pred_chd.qda <- predict(qda.fit, newdata = heart[-train, ])
result.qda <- table(heart$chd[-train], pred_chd.qda$class)
result.qda
# 정분류율
accuracy.qda <- 100*sum(diag(result.qda))/sum(result.qda)
accuracy.qda

###########################################################################
# 군집분석
###########################################################################

# install.packages("kohonen", repos='http://cran.us.r-project.org')

options(repr.plot.width = 5, repr.plot.height = 4) #그림 크기 옵션

crime <- read.csv("3월22일/Crime.csv")
head(crime)

attach(crime)
x  <- crime[,3:4]
D1 <- dist(x)  # 유클리드거리
D1 
round(D1)
plot(x)

D2 <- dist(x, method = "manhattan") # 맨하탄거리
D2

hc1 <- hclust(dist(x)^2, method = "single")   #최단 연결법
plot(hc1, labels = city, hang = 1, main = "dendrogram : 최단 연결법")

hc2 <- hclust(dist(x)^2, method = "complete")   #최장 연결법
plot(hc2, labels = city, hang=1, main = "dendrogram : 최장 연결법")



c1.num <- 3  #군집 수 설정

hc1.result <- cutree(hc1, k = c1.num) #최단 연결법 결과
names(hc1.result) <- crime$city
hc1.result

# 그룹 별 산점도로 결과 확인
plot(x, pch = hc1.result, col = hc1.result, main = "single")
text(x, labels = city, adj = -0.1, cex = 0.8)

hc2.result <- cutree(hc2, k = c1.num) #최장 연결법 결과
names(hc2.result) <- crime$city
hc2.result

# 그룹 별 산점도로 결과 확인
plot(x, pch =  hc2.result, col = hc2.result, main = "complete")
text(x, labels = city, adj = -0.1, cex = 0.8)

####################################################################
#                            kmeans                                #
####################################################################
crime_k <- kmeans(x ,centers = 3) # 3개 군집
attributes(crime_k)
crime_k$cluster

#grouping
clus  <- cbind(city, x, crime_k$cluster)
clus1 <- clus[(clus[,4] == 1), ]
clus2 <- clus[(clus[,4] == 2), ]
clus3 <- clus[(clus[,4] == 3), ]

kc    <- table(crime_k$cluster)
plot(x, pch = crime_k$cluster, col = crime_k$cluster, main = "K-means clustering")
text(x, labels = city, adj = -0.1, cex = 0.8)

####################################################################
#                              som                                 #
####################################################################

library(kohonen)

wine <- read.csv("3월22일/Wine.csv")
head(wine)

wines.sc <- scale(wine)   # 변수들간의 표준화(단위 환산)
set.seed(7)
# somgrid(xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal"))
# xdim, ydim : dimensions of the grid
# topo       : 토폴로지는 각 노드들이 연결되는 방식

wine.som <- som(wines.sc, grid = somgrid(5, 4, "hexagonal"))

plot(wine.som, main="Wine data")
summary(wine.som)
wine.som$unit.classif

par(mfrow=c(2,1))
plot(wine.som, type="counts", main="wine data: counts")
plot(wine.som, type="quality", main="wine data: mapping quality")
# plot(wine.som, type="mapping",  col = wine.som$unit.classif, main="mapping plot")

par(mfrow=c(3,1))
colour1<- tricolor(wine.som$grid)
plot(wine.som, "mapping", bg=rgb(colour1))
colour2<- tricolor(wine.som$grid, phi=c(pi/6, 0, -pi/6))
plot(wine.som, "mapping", bg=rgb(colour2))
colour3<- tricolor(wine.som$grid, phi=c(pi/6, 0, -pi/6), offset= .5)
plot(wine.som, "mapping", bg=rgb(colour3))

library(dbscan)

iris <- read.csv("3월22일/Iris.csv")
iris <- as.matrix(iris[,1:4])
head(iris)
summary(iris)

# kNNdistplot(iris, k = 5)
# abline(h = 0.5, col = "red", lty=2)

res <- dbscan(iris, eps = .42, minPts = 5)
pairs(iris, col = res$cluster + 1L)

newdata <- iris[110:120,] # + rnorm(10, 0, .2)
predict(res, newdata, iris)

