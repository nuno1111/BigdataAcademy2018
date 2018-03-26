#============================================================
#title: "탐색적 자료분석"
#subtitle: "Exploratory Data Analysis"
#author: "Begas"
#date: "2018"
#============================================================

# install.packages("tidyverse")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("maps")

### 예제 데이터
library(tidyverse)
data<-read.csv("3월20일/rainforest.csv")
head(data)

#- dbh : diameter at breast height (가슴높이쯤 직경)
#- wood : wood biomass (목재자원)
#- species : 나무 종

### 산점도 (Scatter Plot)
ggplot(data,aes(wood,dbh))+geom_point(aes(color=species),size=4,
                                      alpha=1/2)+theme_classic()

### 산점도 (Scatter Plot)
ggplot(data,aes(wood,dbh))+geom_point(aes(color=species),size=4,
                                      alpha=0.8)+facet_wrap(~species)

### 선그래프 (Line Plot)
ggplot(data,aes(wood,dbh))+geom_line(aes(color=species),size=2)+
  theme_classic()

### 선그래프 (Line Plot)
ggplot(data,aes(wood,dbh))+geom_line(aes(color=species),size=2)+
  facet_wrap(~species)

### 히스토그램 (Histogram)
ggplot(data,aes(dbh))+geom_histogram(fill="lightblue"
                                     ,color="black",bins=30)+theme_classic()

### 히스토그램 (Histogram)
ggplot(data,aes(dbh))+geom_histogram(aes(fill=species),bins=30)+
  facet_wrap(~species)

### 막대그래프 (Bar Plot)
ggplot(data,aes(species,dbh))+geom_bar(aes(fill=species),
                                       stat = "summary", fun.y = "mean",width=0.7)+theme_classic()

### 상자그림 (Box Plot)
ggplot(data,aes(species,dbh))+geom_boxplot(aes(fill=species))+
  theme_classic()

### 상자그림 (Box Plot) - violin
ggplot(data,aes(species,dbh))+geom_violin(aes(fill=species))+
  theme_classic()

### 원그림 (Pie Chart) data
table(data$species)
T<- data.frame(table(data$species))
T

### 원그림 (Pie Chart)
ggplot(T,aes(x="",y=Freq,fill=Var1))+
  geom_bar(stat="identity",width=1,color="gray20",position="fill")+
  geom_text(aes(y=1.2-cumsum(Freq/sum(Freq)),
                label=paste(round(Freq/sum(Freq)*100,1),"%")))+
  coord_polar("y")+theme_classic()

### 지도그림 (Map) data
states <- map_data("state")
head(states)

### 지도그림 (Map)
ggplot(data = states,aes(x=long,y=lat)) + 
  geom_polygon(aes(group = group), color = "white",fill="skyblue") + 
  guides(fill=FALSE)+theme_classic()

### Word Cloud
library(tm)
library(wordcloud)
library(RColorBrewer)
conventions <- read.table("3월20일/conventions.csv",header = TRUE,sep = ",")
head(conventions)

### Word Cloud
wordcloud(conventions$wordper25k,conventions$democrats, # frequencies
          scale = c(4,1), # size of largest and smallest words
          colors = brewer.pal(9,"Blues"), # number of colors, palette
          rot.per = 0) # proportion of words to rotate 90 degrees

### Word Cloud
wordcloud(conventions$wordper25k, conventions$republicans,
          scale = c(4,1),
          colors = brewer.pal(9,"Reds"),
          rot.per = 0)

