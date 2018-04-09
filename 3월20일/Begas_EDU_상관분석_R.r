#============================================================
# title: "상관분석"
# subtitle: "Correlation Analysis"
# author: "Begas"
# date: "2018"
#============================================================

#============================================================
# 상관분석 예제 1
#============================================================
# install.packages("corrplot")
library(corrplot)

# fitness 데이터(출처: JMP Software 내 예제 데이터)
fitness <- read.csv("3월20일/fitness.csv")
head(fitness, 3)
summary(fitness)
# age: 나이
# weight: 체중
# oxygen: 산소 소비량
# runtime: 1마일을 뛰는데 소요된 시간
# runpulse: 달리는 동안 평균 맥박수
# rstpulse: 휴식기 맥박수
# maxpulse: 달리는 동안 최대 맥박수

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
pairs(fitness, upper.panel = panel.cor, diag.panel = panel.hist)

# 상관계수 행렬
M <- cor(fitness)
print(round(M, 4))

# 상관계수 행렬 시각화
corrplot(M, method = "circle")
corrplot(M, method = "ellipse", order = "hclust")

#============================================================
# 상관분석 예제 2: partial correlation
#============================================================

# (Question) 수입이 높을수록 지출금액도 클까?

dat <- read.csv("3월20일/consumption.csv")
head(dat)
summary(dat)
# consumption: 지출금액($)
# income: 수입($)
# wealth: 재산($)

pairs(dat, upper.panel = panel.cor, diag.panel = NULL)
# 수입은 지출에 큰 영향을 주는 것으로 보임
# 재산이 주는 영향은?

# 상관계수
round(cor(dat), 4)
# cor() 함수 사용
# 지출과 수입의 상관계수는 0.9808

# 편 상관계수
# install.packages("ppcor")
library(ppcor)
round(pcor(dat)$estimate, 4)
# ppcor package의 pcor() 함수 사용
# 재산 통제 후 지출과 수입의 상관계수는 0.3969



