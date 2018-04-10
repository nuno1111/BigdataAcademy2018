
# 분석대상자료 : 소상공인 상권정보 상가업소 데이터 
#                https://www.data.go.kr/dataset/fileDownload.do?atchFileId=FILE_000000001436913&fileDetailSn=1  
# 전국상가 정보중에서 제주도에 해당되는 42,190건의 상가정보의 위도,경도 정보를 이용하여
# 위치의 분포를 파악해봄.
#
# 상권업종대분류명(상권업종대분류코드) - 8개
# 음식(Q), 관광/여가/오락(N), 부동산 (L), 생활서비스(F),소매(D),숙박(O),의료(S),학문/교육(R)
#
# 상권업종중분류명(상권업종중분류코드) - 14개중에서 상위9개만 살펴봄.
# 한식(Q01Axx),중식(Q02Axx),일식/수산물(Q03Axx),분식(Q04Axx),닭/오리요리(Q05Axx),양식(Q06Axx),패스트푸드(Q07Axx),제과제빵떡케익(Q08Axx),
# 유흥주점(Q09Axx),별식/퓨전요리(Q10Axx),커피점/카페(Q12Axx),음식배달서비스(Q13Axx),기타음식업(Q14Axx),부페(Q15Axx)
# 
# "제주상가업소_201703_04.csv" 파일은 아래 폴더에 올렸음.
#  https://drive.google.com/drive/folders/1uZDvPLLDhUS9GCnbsPCMnMRtE8tRHTBU?authuser=0 


# 데이터 로딩 ----
df_store <- read.csv("./파일럿프로젝트/JEJU_STORE.csv")

# 데이터 탐색 
head(df_store)
str(df_store)
summary(df_store)

# 상하 섬지역 제외 
df_store <- subset(df_store,(df_store$"위도"< 33.8) & (df_store$"위도" > 33.2))


# 상권업종대분류명(상권업종대분류코드) - 8개
# 음식(Q), 관광/여가/오락(N), 부동산 (L), 생활서비스(F),소매(D),숙박(O),의료(S),학문/교육(R)
xcode_store <- c("음식","Q", "관광","N", "부동산","L", "생활서비스","F","소매","D","숙박","O","의료","S","교육","R")
dim(xcode_store) <- c(2,8)


# 대분류별 상가 분포(위치) 파악
div <- par(mfrow = c(3,3))  # 가로 3줄, 세로 3줄로 화면을 분할함

plot(df_store$경도,df_store$위도,main="Total store", pch = 1, cex = .6) #전체 상가 출력
for (loop_i in seq( ncol(xcode_store) ) ) {
  tmp_store <- subset(df_store,df_store$"상권업종대분류코드"==xcode_store[2,loop_i])
  plot(tmp_store$경도,tmp_store$위도,main = paste( xcode_store[1,loop_i], "store"), pch = 1, cex = .6)
}
par(div)  # 화면 분할을 종료함


# 상권업종중분류명(상권업종중분류코드) - 14개
# 한식(Q01Axx),중식(Q02Axx),일식/수산물(Q03Axx),분식(Q04Axx),닭/오리요리(Q05Axx),양식(Q06Axx),패스트푸드(Q07Axx),제과제빵떡케익(Q08Axx),
# 유흥주점(Q09Axx),별식/퓨전요리(Q10Axx),커피점/카페(Q12Axx),음식배달서비스(Q13Axx),기타음식업(Q14Axx),부페(Q15Axx)
xcode_mid_store <- c("한식","Q01","중식","Q02","일식","Q03","분식","Q04","닭","Q05","양식","Q06","패스트푸드","Q07","제과떡","Q08","유흥주점","Q09","퓨전요리","Q10","커피점","Q12","음식배달","Q13","기타","Q14","부페","Q15")
dim(xcode_mid_store) <- c(2,14)
xcode_mid_store<-t(xcode_mid_store)
xcode_mid_store[,c(1,2)] <- xcode_mid_store[,c(2,1)]
colnames(xcode_mid_store) <- c("code","name")
xcode_mid_store <- data.frame(xcode_mid_store)  #data frame으로 변환


#상위 9개만 선택
library(sqldf)
cnt_by_code_mid <- sqldf("
                         SELECT 
                         substr(상권업종중분류코드,1,3),
                         count(*)
                         value
                         FROM df_store
                         WHERE substr(상권업종중분류코드,1,1) = 'Q'
                         GROUP BY substr(상권업종중분류코드,1,3)
                         ORDER BY count(*) desc
                         ")
colnames(cnt_by_code_mid) <- c("code","cnt")
cnt_by_code_mid <- cnt_by_code_mid[1:9,]
xcode_mid_store <- merge(xcode_mid_store,cnt_by_code_mid,by="code")

# 중분류별 상가 분포(위치) 파악
div <- par(mfrow = c(3,3))  # 가로 3줄, 세로 3줄로 화면을 분할함

for (loop_i in seq( nrow(xcode_mid_store) ) ) {
  tmp_store <- subset(df_store,df_store$"상권업종중분류코드" == as.character(xcode_mid_store$code[loop_i]))
  plot(tmp_store$경도,tmp_store$위도,main = paste( as.character(xcode_mid_store$name[loop_i]), "store"), pch = 1, cex = .6)
}
par(div)  # 화면 분할을 종료함



