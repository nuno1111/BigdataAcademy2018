#getwd 함수를 사용하여 현재 작업공간 위치를 알 수 있음
print(getwd())

#setwd 함수를 사용하여 사용자가 사용할 작업 공간 위치를 설정 할 수 있음
setwd('C:\\Users\\begas\\RProject')

#ls 함수를 사용하여 현재 작업공간의 오브젝트 리스트를 확인 할 수 있
ls()

#현재 사용하고 있는 작업공간 저장
save.image() 

#지정한 것만 저장 시
#save(object list, file = 'myfile.RData') 

#작업공간 불러오기
load('myfile.RData')

#print함수를 사용하여 출력
print(summary(iris))

head(iris)
head(iris,10)

tail(iris)
tail(iris,10)

#cat 함수는 print의 대안으로 여려 개의 항목을 묶어서 연결된 결과를 출력해 줌
#줄을 끝내려면 꼭 줄 바꿈 문자(\n)을 넣어야 함

cat('Cat 함수 예제 : 1더기 5의 값은 ? ', 1+5, '\n')
cat('오늘의 날짜는 ? ', date(), '\n')

x <- 10
y <- 10
z <- c(1,2,3,4,5)

#생성된 변수 목록을 보여줌
#ls함수에서는 목록만 보여주며 변수의 유형 값 등을 확인 할 수 없음
ls()

#ls.str함수는 생성된 변수 목록 뿐만 아니라 각 변수의 유형 및 값 등을 확인 가능
ls.str()

x <- 10
y <- 20
z <- 30
ls()  #생성된 변수 보기

rm(x)  #x변수 삭제

rm(list = ls()) #오브젝트 내의 모든 변수 삭제

gc() # 메모리 확인 변수

x <- 10
cat("x: ", x, "\n")

y <- c(1, 2, 3, 4, 5)
cat("y: ", y, "\n")

z <- c(x, y)
cat("z: ", z, "\n")

a <- c('one', 'two', 'three')
cat("a: ", a, "\n")

b <- c(TRUE, FALSE, TRUE)
cat("b: ", b, "\n")

c <- y-x
cat("c: ", c, "\n")

d <- c(y, a)
cat("d: ", d, "\n")

# 데이터타입 강제형변환 우선순위 
c(T, 1) # bool < num
c(1, 1i) # num < complex
c(1i, "one") # complex < string

# 데이터 타입 확인
is.numeric(1)
mode(1)
class(1)

# NA : 결측값 (있어야 하는데 없는 것)
# null : 데이터 유형과 자료의 길이도 0인 비어있는 값(애초에 없는 값)
# NaN : 수학적으로 정의가 불가한 수

#CSV 파일 가져오기 예시 1: read.csv()로 csv file import
cat(paste0(rep("=",10), collapse = "="), "read.csv()", paste0(rep("=",10), collapse = "="),"\n")
system.time(DF1 <-read.csv('test_1.csv', stringsAsFactors=FALSE))
DF1

#CSV 파일 가져오기 예시 2 : read.table()로 csv file import
cat(paste0(rep("=",10), collapse = "="), "read.table()", paste0(rep("=",10), collapse = "="),"\n")
system.time(DF2 <- read.table('test_1.csv', header=TRUE, sep=',', quote='',
                              stringsAsFactors=FALSE, comment.char='',
                              colClasses=c('integer', 'integer', 'numeric',
                                           'character', 'numeric', 'integer')))
DF2

#CSV 파일 가져오기 예시 3 : data.table::fread()로 csv file import.
library(data.table)
cat(paste0(rep("=",10), collapse = "="), "data.table::fread()", paste0(rep("=",10), collapse = "="),"\n")
system.time(DT1 <- fread('test.csv'))


#CSV 파일 가져오기 예시 4: sqldf::read.csv.sql()로 csv file import
library(sqldf)
cat(paste0(rep("=",10), collapse = "="), "read.csv.sql()", paste0(rep("=",10), collapse = "="),"\n")
system.time(SQLDF <- read.csv.sql('test.csv', dbname=NULL))

#Sample Data 생성
n <- 1e6
DT <- data.frame(a=sample(1:1000, n, replace=TRUE),
                 b=sample(1:1000, n, replace=TRUE),
                 c=rnorm(n),
                 d=sample(c('foo', 'bar', 'baz', 'qux', 'quux'), n, replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000, n, replace=TRUE))
summary(DT)

# CSV 파일로 저장하기 예시 1
write.table(DT, 'test_1.csv', sep=',', row.names=FALSE, quote=FALSE)

# CSV 파일로 저장하기 예시 2
write.csv(DT, 'test_2.csv', row.names=FALSE, quote=FALSE)
print("done")

seq(1, 7, by=2)                #from, to, by, length, along
seq(1, -1, by=-0.5)
seq(1, 7, length=3)
1:7 # = seq(1, 7)

rev(seq(1:5))                   #rev : 자료의 순서를 역순으로 만드는 함수, 5:1

rep(c(1,2,3), 3)               #rep(a, b)는 a를 b만큼 반복
rep(1:3, 3)                       #a:b는 a부터 b까지의 수
rep(c(4, 2), times=2)
rep(c(4, 2), times=c(2, 1))

paste('no', 1:3)               #반복되는 문자에 첨자를 붙여줌
paste0('no', 1:3)               #반복되는 문자에 첨자를 붙여줌 + 간격 생략

v1 <- c(1, 2, 3)                  #숫자형 벡터 생성
v2 <- c('a', 'b', 'c')            #문자형 벡터 생성
v3 <- c(T, F, T)                  #논리형 벡터 생성

height <- c(160, 140, 155)        #height 벡터 생성
people <- c('Ned', 'Jill', 'Pat') 
names(height) <- people           #height 벡터의 원소에 이름을 할당

height[1]
height['Ned']
is.numeric(height)
names(height) <- NULL             #height 벡터의 원소의 이름을 삭제

vec1 <- c(1, 2, 3, 4, 5)  #1~5까지 자료를 갖는 vec1 변수 생성
vec1[2]     #두 번째 자료
vec1[c(2, 3, 5)]    #vec1의 2, 3, 5의 값만 표현
vec1[c(-2, -3)]    #vec1의 2, 3번째 자료 값 삭제
vec1 > 2
vec1[vec1 > 2]    #vec1에서 2보다 큰 값만 표현
vec1[2] <- 6    #두 번째 위치의 2값이 6으로 대체됨
replace(vec1, 3, 2)   #vec1의 세 번째 자료를 2로 변경 replace(벡터, 위치, 값)
append(vec1, 8, after=5)    #vec1의 5번째 자료 다음에 8을 삽입  append(벡터, 값, after=위치)
v1 <- 1:3
v2 <- 2:4
v1 * v2   #벡터의 각 원소간 곱셈

#행렬(matrix)은 여러 변수들이 이차원적으로 모여 있는 개체로, 
#행렬을 생성하기 위해서는 matrix() 함수를 사용
#matrix() 함수 이외에 cbind(), rbind(), dim() 등을 이용하여 행렬을 생성시킬 수 있음

matrix(1:9, nrow=3)                                    #nrow  : 행의 개수 지정
matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), byrow=T, ncol=3)  #ncol : 열의 개수 지정 byrow=T : 행 기준 행렬을 생성

r1 <- c(1, 4, 7)                                       #r1, r2, r3 행 벡터 생성
r2 <- c(2, 5, 8)
r3 <- c(3, 6, 9)

rbind(r1, r2, r3)                                      #rbind : 행을 기준으로 결합
cbind(r1, r2, r3)                                      #cbind : 열을 기준으로 결합

m1 <- 1:9
dim(m1) <- c(3, 3)

#행렬과 관련된 여러 함수와 성분의 추출과 삭제 등에 관해 알아봄
mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol=3, byrow=T) #행 기준 3열의 행렬 생성
mat[1,]                                       #행렬 mat의 1행의 값
mat[,3]                                        #행렬 mat의 3열의 값
mat[mat[,3] > 4, 1]                           #3열에서 4보다 큰 행의 값 중 1열의 모든 값

mat[mat[,3] > 4, 2]                           #3열에서 4보다 큰 행의 값 중 2열의 모든 값
mat[2,, drop=F]                               #2행 값만을 행렬 형태로 추출
is.matrix(mat[2,, drop=F])                    #mat[2,,drop=F]가 행렬인지 확인
is.matrix(mat[2,])

#배열의 속성 :  행렬의 속성과 같이 자료의 개수를 나타내는 length, 형태를 보여주는 mode, 
#각 차원의 벡터의 크기를 나타내는 dim 그리고 각 차원의 리스트 이름을 나타내는 dimnames로 구성

#배열의 생성
#배열을 생성하기 위한 함수로 array() 함수와 dim() 함수가 있음
array(1:6)                                      #1~6의 자료로 1차원 배열 생성
array(1:6, c(2, 3))                             #1~6의 자료로 2차원 배열 생성
array(1:8, c(2, 2, 2))                          #1~8의 자료로 3차원 배열 생성

arr <- c(1:24)                                 #1~24의 자료 생성
dim(arr) <- c(3, 4, 2)                        #dim() 함수를 이용하여 3행 4열의 행렬 2개 생성

#배열의 연산
ary1 <- array(1:8, dim = c(2, 2, 2))  
ary2 <- array(8:1, dim = c(2, 2, 2))
ary1 + ary2                                     #자료의 덧셈
ary1 * ary2                                     #자료의 곱셈
ary1 %*% ary2                                   #두 배열 원소들의 곱의 합
sum(ary1 * ary2)                                #ary1 %*% ary2 와 같은 결과를 냄

#배열원소의 추출 및 삭제
ary1[,,1]
ary1[1,1,]
ary1[1,,-2]

#data.frame() : 이미 생성되어 있는 벡터들을 결합하여 데이터 프레임을 생성
char1 <- rep(LETTERS[1:3], c(2, 2, 1))            #벡터 char1
num1 <- rep(1:3, c(2, 2, 1))                      #벡터 num1
test1 <- data.frame(char1, num1)                  #test1 데이터 프레임 생성

#as.data.frame() :모든 다른 종류의 자료객체들을 데이터 프레임으로 변환
a1 <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')
dim(a1) <- c(5,3)
test3 <- as.data.frame(a1)                        #a1 메트릭스를 데이터 프레임으로 변환

df1 <- data.frame(Col1 = c('A', 'B', 'C'), Col2 = c(1, 2, 3), Col3 = c(3, 2, 1))
#df1[행, 열]
df1[ , 'Col3']     #결과 : 3, 2, 1 출력
df1[1, ]           #결과 : A  1  3 출력
df1[3, 'Col1']     #결과 : C 출력

lst <- list('top', c(2, 4, 6), c(T, F, T))                #list(문자, 숫자, 논리형 객체) 
lst[[1]]                                                  #[[1]] 첫 번째 성분
lst[1]                                                    #[1] 첫 번째 리스트

mat1 <- matrix(1:4, nrow=2)
list1 <- list('A', 1:8, mat1)
son <- list(son.name = c('Minsu', 'Minchul'), son.cnt = 2, son.age = c(2, 6))

#리스트 속성 : 벡터의 속성과 같이 자료의 개수, 형태, 구성요소의 이름 등을 보여주는 length, mode, names로 구성
length(son)                                   #son 리스트 자료의 개수
mode(son)                                    #son 리스트 자료의 형태
names(son)                                   #son 리스트 각 구성요소의 이름

#예제1(생성)
exm <- list(c('Abe', 'Bob', 'Carol', 'Deb'), c('Weight', 'Waist'))  #exm의 이름으로 list생성
exm[[2]]                                             #리스트의 2번째 성분
exm[[2]][2]                                          #2번째 성분 2번째 원소
names(exm) <- c('Rows','Columns')           #exm 리스트에 성분 이름 부여
exm$Rows                                          #exm의 Rows 성분만 표현
exm$Rows[2]                                       #Rows 성분 2번째 원소 표현
exm$Columns                                     #exm의 Columns 성분만 표현

#관련함수
#리스트는 성분에 리스트와 벡터 등을 사용할 수 있음
#예제1
a <- 1:10
b <- 11:15                                     
klist <- list(vec1=a, vec2=b, descrip='example')
klist[[2]][5]                                      #두 번째 성분 vec2의 5번째 원소
klist$vec2[c(2, 3)]                             #vec2의 2, 3번째 원소

#스칼라 연산
#산술연산자(사칙연산)
1 + 2 
x <- 3
y <- 2
x + y
x - y
x * y
x / y

#벡터의 연산
a <- c(1, 2)
b <- c(3, 4)
a + b #벡터 변수의 덧셈
a - b #벡터 변수의 뺄셈
a * b #벡터 변수의 곱셈
a / b #벡터 변수의 나눗셈

#행렬 연산
A<-matrix(c(5, 10, 2, 1), ncol=2)
B<-matrix(c(3, 4, 5, 6), ncol=2)
A + B #행렬변수의 덧셈
A - B #행렬변수의 뺄셈
A * B #행렬변수의 곱셈
A / B #행렬변수의 나눗셈

2^2 #일반적인 숫자의 제곱(2의 2제곱)
x <- 4
y <- 3
x^y #값을 변수에 할당한 후에 제곱(4의 세제곱)

a <- c(4, 5)
b <- c(2, 3)
a^b #벡터변수의 제곱(구성 요소들간의 제곱, 4^2, 5^3)

A <- matrix(c(5, 10, 2, 1), ncol=2)
B <- matrix(c(2, 2, 2, 2), ncol=2)
A^B #행렬변수의 제곱(각 구성 요소들간의 제곱)

#%/% : 나눗셈에서 몫만 출력함
4 %/% 2 
x <- 3
y <- 2
x %/% y #1.5의 정수부분인 1만 출력

a <- c(7, 2)
b <- c(3, 4)
a %/% b #벡터 변수의 정수나눗셈

A <- matrix(c(5, 10, 2, 1), ncol=2)
B <- matrix(c(3, 4, 5, 6), ncol=2)
A %/% B #행렬변수의 정수나눗셈

#행렬의 곱
A <- matrix(c(5, 10, 2, 1), ncol=2)
B <- matrix(c(3, 4, 5, 6), ncol=2)
#(5*3) + (2*4) ; (5*5) + (2*6) ; (10*3) + (1*4) ; (10*5) + (1*6)
A %*% B

# '==' 비교되는 두 항이 같은지를 비교함. 같을 경우 True, 다를 경우 False
1 == 2
x <- 2
y <- 3
x == y

# '!=' 비교되는 두 항이 다른지를 비교함. 같을 경우 False, 다를 경우 True
1 != 2
x <- 2
y <- 3
x != y

# '<=' 왼쪽 항이 오른쪽 항보다 작거나 같음을 비교함. 작거나 같으면 True, 크면 False
1 <= 2
x <- 2
y <- 2
x <= y

# '<' 왼쪽 항이 오른쪽 항보다 작음을 비교함. 작으면 True, 크면 False
1 < 2

# '>' 왼쪽 항이 오른쪽 항보다 큼을 비교함. 크면 True, 작으면 False
1 > 2

# '>=' 왼쪽 항이 오른쪽 항보다 크거나 같음을 비교함. 크거나 같으면 True, 작으면 False
1 >= 2


# && : 일반적인 and 논리 연산자 (스칼라)
2 == 2 && 3 > 4
# & : 벡터에서의 and 논리 연산자
2 == 2 && c(2 == 2, 3 > 4) #벡터에서 && 연산자를 잘못 사용한 사례
2 == 2 & c(2 == 2, 3 > 4)
# || : 일반적인 or 논리 연산자 (스칼라)
2 == 2 || 3 > 4
# | : 벡터에서의 or 논리 연산자
2 != 2 || c(2 == 2, 3 > 4) #벡터에서 || 연산자를 잘못 사용한 사례
2 != 2 | c(2 == 2, 3 > 4)
# ! : not 연산자
!TRUE

tmp_df <- data.frame(AA = c(1:5), BB = c('A', 'A', 'B', 'B', 'B'))

names(tmp_df)      #열의 이름의 결과 출력

colnames(tmp_df)  #열의 이름의 결과 출력

rownames(tmp_df) #행의 이름의 결과 출력

dimnames(tmp_df) #행과 열의 이름의 결과를 List로 출력

names(tmp_df)[1] <- 'AA_1'           #첫번째 열의 이름을 변경
#[1] 'AA_1' 'BB'  

names(tmp_df) <- c('AA_2', 'BB_2')  #첫번째 열과 두번째 열의 이름을 변경
#[1] 'AA_2' 'BB_2'

colnames(tmp_df)[1] <- 'AA'          #첫번째 열의 이름을 변경
#[1] 'AA'   'BB_2'

rownames(tmp_df)[1] <- 'row1'       #첫번째 행의 이름을 변경
#[1] 'row1' '2'    '3'    '4'    '5' 

str(tmp_df) # df의 구조 정보

tmp_df <- data.frame(AA = c(1:5), BB = c('A', 'A', 'B', 'B', 'B'))
tmp_df

#CC컬럼을 새로 생성하고 그 안에 값을 1로 채워 넣음 
tmp_df$CC <- 1
tmp_df

#컬럼 AA와 컬럼 CC의 값의 합한 값을 새로운 DD컬럼으로 생성
tmp_df$DD <- tmp_df$AA + tmp_df$CC
tmp_df

tmp_df[, -1]     #첫번째 위치의 컬럼 제거

#tmp_df[, -'AA']  #오류 발생함

tmp_df[, c('BB', 'CC', 'DD')]

subset(tmp_df, select = -AA)   #subset(df, WHERE, select) * 생각보다 유용한 함수 

subset(tmp_df, select = -c(AA, BB))

subset(tmp_df, AA > 3,select = -c(AA, BB))

Sample.df <- data.frame(AA = rep(letters[1:5], 10), BB = sample(60:70, 50, replace=T), stringsAsFactors=FALSE)
#AA컬럼의 값중에서 a인 값만 추출
#Type1
Sample.df[Sample.df$AA == 'a',] 
#Type2
subset(Sample.df, AA == 'a') 

#AA컬럼의 값중에서 a 와 b의 값만 추출
#Type1
Sample.df[Sample.df$AA %in% c('a','b'),]
#Type2
subset(Sample.df, AA %in% c('a','b'))

# 필요한 컬럼 Select
tmp_df1 <- tmp_df
#Type1
tmp_df1[,c('AA', 'BB', 'CC')]
#Type2
tmp_df1[,c(1, 2, 3)]
#Type3
tmp_df1[,c(-4, -5)]
#Type4
subset(tmp_df1, select = c('AA', 'BB', 'CC'))
#Type5
subset(tmp_df1, select = c(AA, BB, CC))
#Type6
subset(tmp_df1,select = c(-CC))

#컬럼을 합치기 위해서는 cbind함수를 사용한다.
#단, 합칠 대상의 데이터의 행 개수는 동일 하여야 한다.
#데이터 프레임인 경우 합칠 두 개의  데이터유형이 달라도 상관 없지만, matrix인 경우 데이터 유형이 같아야 한다.

aa <- c(1:5)
bb <- c(5:1)
cbind(aa, bb)

tmp_df <- data.frame(AA = c(1:5), BB = letters[1:5])
cbind(tmp_df, aa)

#행을 합치기 위해서는 rbind함수를 사용
#단, 합칠 대상의 열 개수는 동일 해야 함
#데이터 프레임인 경우 합칠 두개의 데이터의 각각의 열의 유형이 동일해야 함

rbind(aa, bb)



tmp_df1 <- data.frame(AA = c(1:2), BB = c('A', 'A'))
tmp_df2 <- data.frame(AA = c(3:4), BB = c('B', 'B'))
rbind(tmp_df1, tmp_df2)

# 공통된 열을 하나이상 가지고 있는 두 데이터 프레임에 대하여 공통된 열에 맞춰 행들을 하나의 데이터 프레임으로 병합하기 위해서는 merge 함수를 사용한다.

tmp1 <- data.frame(AA = c('A', 'A', 'B'), BB = c(1, 2, 3))
tmp2 <- data.frame(AA = c('A', 'C'), CC = c('OLD', 'NEW'))

#tmp3 기준으로 데이터 합치기 : inner join
merge(tmp1, tmp2, by = 'AA')
merge(tmp1, tmp2, by = 'AA', all = F)

#tmp1기준으로 데이터 합치기 : left join
merge(tmp1, tmp2, by = 'AA', all.x = T)

#tmp2 기준으로 데이터 합치기 : right join
merge(tmp1, tmp2, by = 'AA', all.y = T)

#tmp3 기준으로 데이터 합치기 : outer join
merge(tmp1, tmp2, by = 'AA', all = T)



# 집단별 데이터를 분류하기 위해서는 split 함수 사용
# 집단별로 분류된 데이터는 list 형태로 생성됨
split(tmp1, tmp1$AA)

#문장을 나눌 때는 strsplit 함수를 사용
strsplit('A text I want to display with spaces', ' ')

a <- c(1, 4, 5, 3, 5, 3, 7) #벡터형태에서는 sort 함수를 사용하여 정렬
sort(a) 

order(a) #값의 순위값을 나타낸다

sort(a, decreasing = T)

order(a, decreasing = T)

tmp1 <- data.frame(AA = c('A','A','B','C','D'), BB = c(5,3,6,1,2))
tmp1[order(tmp1$BB),] #오름차순 정렬

tmp1[order(tmp1$BB, decreasing = T), ] #내림차순 정렬


#특정한 조건을 만족했을 경우에만 프로그램 코드를 수행하는 제어 구문. 항상 논리 연산이 수반 된다
#if(조건) 실행문
x <- c(1, 2, 3, 4); y <- c(2, 1, 4, 5)
if(sum(x) < sum(y)) print(x)     #x의 합이 y의 합보다 작을 경우 실행

#if(조건) {
#    조건이 True 일때 실행문
# } else {
#    조건이 False 일때 실행문
# }
x <- c(1,2,3,4)
y <- c(2,1,4,5)
if(mean(x)>mean(y)) print('Mean(x)>Mean(y)') else print('Mean(x)<Mean(y)')

if(mean(x)>mean(y)){
  print('Mean(x)>Mean(y)') 
}else{ 
  print('Mean(x)<Mean(y)')
}


#중첩 조건문 : 조건문 안에 조건문이 있는 표현식
if(length(x) == 5) {
  if(sum(x) == 10) print('length=5, sum=10')
} else {
  print('length=4, sum=10')
}

#ifelse(조건, 조건이 참일때의 실행문, 조건이 참이 아닐때의 실행문)
ifelse(x<y, x, y)
ifelse(sum(x-y) > 0, 'positive', ifelse(sum(x-y) < 0 , 'negative', 'zero'))

#for(변수 in 반복횟수) 실행문 : 실행문을 반복횟수만큼 실행

#예제1
#변수 i가 1에서 5까지의 값을 갖을 때까지 print(rep(i,i))라는 실행문을 실행한다. 
#i=1이면 print(rep(1,1))을 실행하고 i=2이면 print(rep(2,2))을 실행한다. 이렇게 i=5일때까지 실행을 하게된다.
for(i in 1:5) print(rep(i, i))

#예제2 : 1부터 10까지 합 구하기
sum.x <- 0
for(i in 1:10) sum.x <- sum.x + i
sum.x
