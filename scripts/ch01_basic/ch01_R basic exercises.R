##### ------------------------------ ####
# my session informations

# title: R basic grammer
# date: 2022.08.09
# author: Ph.D. Y-H Lim
# R version 4.2.1 (2022-06-23 ucrt)/ # Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22000)

# package management -----------------------

# Library path 확인
.libPaths() 

# 설치 경로 변경, 사용자 패키지, 주로 4.1.1버전에 설치하였음
.libPaths("C:/Program Files/R/R-4.1.1/library") # 이건 실행되지 않게 하세요.

# Loading my packages
library(dplyr)
library(ggplot2)
##### ------------------------------ ####


# 이 코드는 한 줄씩 실행해 가면서 R을 경험하도록 만들었습니다. 
# 당장 코드 하나하나가 이해되지 않아도, 
# 어떻게 작동하는지, 어떤 결과를 보여주는지, 
# 그리고 어떠한 방식으로 R을 활용할 수 있는지 알게 될 것입니다. 
# 모든 일이 그렇지만, 처음부터 모든 것을 알고 시작하는 사람은 없습니다. 
# 해보면 나에게 필요한 것을 R을 통해 만들 수 있습니다. 




#0. 들어가기  --------------------------

4 + 2 # R은 line by line 형태로 실행됨. 실행하려면 "Ctrl + Enter"
# 결과는 콘솔(CONSOLE)에 나타남

# "#" symbol 이후에 모든 것은 R이 무시, 그래서 "#" 이후에 주석을 달면 좋음

# 값(value)을 객체(object)에 할당(ASSIGN)할 때 "<-" 기호 사용 (shortcut alt+-)
# 할당 후 object는 "ENVIRONMENT"에 나타남)
a <- 12


# 이름으로 코드 실행하면, CONSOLE에 내용을 프린트 
a

# 위의 객체(object)를 값(value)으로 사용할 수도 있음

# "a" 값은 변하지 않음, 
# 결과가 저장되지도 않음

b <- 7

# 사칙연산
a + b
a - b
a * b
a/b

# 연산 결과를 새로운 객체에 할당(저장)
c1 <- a + b
c2 <- a * b
c3 <- b - a

c1 <- a + b # 콘솔에 프린트하지 않고 assign
c1          # print

(c <- a + b) # 할당과 프린트 동시에 하는 법





#1. 연산자(Operators)  --------------------------

#   +   더하기(addition)
#   -   빼기(subtraction)
#   *   곱하기(multiplication)
#   /   나누기(division)
#   ^   제곱(power)
#   %%  나누기 후 나머지 구하기, 7 %% 2 는  1
#   %/% 정수 나눗셈, 9 %/% 4 는 2

#2. 계산 순서 -------------

a + b / 2
a + (b / 2)
(a + b) / 2





#3. 기본 수학 함수들  --------------------------
#   sqrt(x)     제곱근(square root)
#   abs(x)      절대값(absolute value)
#   exp(x)      x를 지수 변환(exponent)
#   log(x)      자연로그(natural logaritm)
#   log10(x)
#   ceiling(x)  x 보다 크거나 같은 정수 (지붕 정수)
#   floor(x)    x 보다 작거나 같은 정수
#   round(x)    x 를 소수점 n자리로 반올림 : round(x, digits=n)
#   trunc(x)    x 소숫점 이하는 잘라서 버림

sqrt(a) 
abs(c3)     
exp(a)     
log(a)     
log10(a)
ceiling(b/2) 
floor(b/2)   
round(b/2, digits = 1)   
trunc(b/2)   

# 함수 도움말 검색
exp?





#4. 객체의 유형들  --------------------------
## 4-1. 데이터형(data type)  --------------------------

### 수치형(numeric)
a <- 1      # 실수형(double)

typeof(a) 

b <- 3L     # 정수형(integer)

typeof(b) 

## 문자형(character)
c <- "헤어질 결심"

typeof(c)   # 문자형(integer)

# character
group1 <- "control"
group2 <- "experimental"
group1 / a   # error. 


# 논리형(logical)
mode(TRUE)    # or mode(T)

typeof(TRUE) # 논리형(logical)

d_double <- c(1,3, 2, 5)
d_integer <- c(2L, 3L, 2L, 1L)
d_char <- c("Trumph", "Joe", "탕웨이", "박해일")
d_logi <- c(T, T, F, T)

# 객체 유형 확인
typeof(d_double)
typeof(d_integer)
typeof(d_char)
typeof(d_logi)

# `is.*()` 함수는 객체가 어떤 타입인지 테스트
is.logical(d_double)
is.logical("TRUE")
is.logical(TRUE)
is.numeric(a)
is.character(d_char)

# 논리 연산
a > b
b > a
a = 2
a == 3
a != 5
a < 12 & b > 7
a < 12 | b > 7





#5. 데이터 구조(data structure)  --------------------------

##5-1 벡터(vector)  --------------------------

# # 원자벡터(atomic vectors)는 같은 유형의 요소들만 포함
c(1, 2, "3") # 모두 캐릭터로 강제변환


# 벡터는 연쇄적인 값들(values)
# c() function으로 만듬. 

# 수치형 벡터
a <- c(1,2,3,4,5)

class(a)
str(a)

# grades and students
math_grades <- c(62, 22, 95, 70, 99, 100)
science_grades <- c(55, 20, 100, 79, 100, 85)
mid_terms <- c(85, 60, 75, 88, 92, 70)
students <- c("김ㅇㅇ", "허ㅇㅇ", "이ㅇㅇ", "한ㅇㅇ", "팽ㅇㅇ", "현ㅇㅇ") # 문자형 벡터

# 벡터를 요약할 수 있는 기본 함수들
is.numeric(math_grades)
length(math_grades)

sum(math_grades)
mean(math_grades)
sd(math_grades)
max(math_grades)
min(math_grades)
range(math_grades)
median(math_grades)

rank(math_grades)
quantile(math_grades)

hist(math_grades)


# 어떤 함수들은 하나 이상의 인수가 필요(ARGUMENT)
# 이 함수들은 2개의 인수(arguments)를 요구, both numeric vectors
cor(math_grades, science_grades) # 상관계수
cov(math_grades, science_grades) # 공변량
plot(math_grades, science_grades) # R 내장 그래프


# 벡터 만드는 방법
# c(), :, seq(), rep()

x1 <- c(1, 2, 3)
x1

x2 <- 1:100
x2

seq(from = 1, to=10, by = 0.5)

rep(1:2, times = 5)
rep("A", times = 10)
rep(c("Anxiety_group", "Control_group"), each = 5, times = 2)
c(rep(c("AD", "MCI", "Control"), each = 5), rep(c("AD", "MCI", "Control"), each = 5))





##5-2 요인(factor)  --------------------------

# 요인은 범주 벡터
# 문자 벡터와 달리, 요인은 "수준(levels)"이 있음

sex <- sample(c("F", "M"), size = 10, replace = TRUE)

x <- factor(sex)

factor(sex, levels = c("M", "F")) # 수준의 순서 변경
factor(sex, levels = c("m", "f")) # 대소문자 조심
factor(sex, levels = c("M"))

# 레이블 변경
factor(sex, labels = c("Female", "Male"))
factor(sex, levels = c("M", "F"), labels = c("Male", "Female"))




##5-3 행렬(matrix)  --------------------------

# matrix(data, nrow, ncol, byrow, dimnames)
matrix(data = c(3,4,5,6,7,8), nrow = 2) # 행 기준 2개의 행으로 매트릭스 생성
matrix(data = c(3,4,5,6,7,8), ncol = 3) # 열 기준 3개의 열로 매트릭스 생성

# 데이터 객체에 저장, 두 매트릭스는 동일일
mt1 <- matrix(data = c(3,4,5,6,7,8), nrow = 2)
mt2 <- matrix(data = c(3,4,5,6,7,8), ncol = 3)

dim(mt1) # 2차원 데이터의 차원 정보
dim(mt2)

mode(mt1)
typeof(mt1)
class(mt1)
str(mt1)

# byrow - 행렬의 값이 생성되는 순서 변경
# "byrow = TRUE" 데이터가 열 우선으로 생성됨 
matrix(data = c(3,4,5,6,7,8), nrow = 2, ncol = 3, byrow = TRUE)

# 행이름, 열이름 부여
mat <- matrix(data = c(3,4,5,6,7,8), 
       nrow = 2, ncol = 3, 
       byrow = TRUE, 
       dimnames = list(c("1st", "2nd"), c("레드벨벳", "블랙핑크", "네이키드비비")))

mat

# 인덱싱(데이터 요소에 접근하는 법)
mat[1,2]            # 1행, 2열의 요소만 출력
mat[1, c(1, 2, 3)]  # 1행의 1,2,3열만 출력
mat[1,]             # 1행만 출력
mat[,c(1,2)]        # 1, 2열만 출력
mat[,-c(1,2)]       # 모든 행 출력, 첫째 두번째 열은 제외 후 출력





##5-4 데이터프레임(dataframe)  --------------------------
# 엑셀 데이터와 가장 유사한 데이터 유형
# 데이터 유형(수치, 문자, 논리...)이 달라도 저장할 수 있음
# R에서 가장 많이 사용하던 데이터셋 유형

# grades and students
math_grades <- c(62, 22, 95, 70, 99, 100)
science_grades <- c(55, 20, 100, 79, 100, 85)
mid_terms <- c(85, 60, 75, 88, 92, 70)
students <- c("김ㅇㅇ", "허ㅇㅇ", "이ㅇㅇ", "한ㅇㅇ", "팽ㅇㅇ", "현ㅇㅇ") # 문자형 벡터


# 벡터 절에 있던 데이터로 데이터프레임 만들기
class_grades <- data.frame(students, math_grades, science_grades, mid_terms)

class_grades

# 데이터 프레임 정보
dim(class_grades) 
mode(class_grades)
typeof(class_grades)
class(class_grades)
str(class_grades)

# 변수 추가
class_grades$group <- c("1반", "2반", "1반", "1반", "2반", "2반")

str(class_grades)

# 데이터 프레임은 행의 수가 같아야 함
class_grades$false_row_n <- c(sample(x=1:7, size = 7, replace = TRUE)) # 새로운 벡터의 행 수가 데이터 프레임의 행 수와 다름


# 데이터 선택(열선택 기준)

## 데이터 프레임식 출력
class_grades[2]               # 위치로 선택; 열좌표
class_grades["math_grades"]   # 이름으로 선택
class_grades[2]               # 인덱스로 선택

## 벡터 출력
class_grades[,2]              # 위치로 선택; 열좌표
class_grades$math_grades      # 이름으로 선택
class_grades[[2]]             # 인덱스로 선택

# drop=FALSE: 벡터 출력을 데이터 프레임식으로 출력


# 데이터 선택(행선택 기준)
## 데이터 프레임식 출력
class_grades[1,]               # 위치로 선택; 행좌표
class_grades["1",]             # 이름으로 선택; 1행
class_grades[c(1,3),]         # 이름으로 선택; 1, 3행

# 논리연산/ 비교 조건식으로 값 추출
# [ (조건식) , (추출 범위) ]
# math_grades가 70이상 이고 science_grades가 80이상인 
# students 열의 값 추출
class_grades[class_grades$math_grades >= 70 & class_grades$science_grades >= 80, "students"]

#위와 같은 조건으로 "students"열과 "group"열 추출
class_grades[class_grades$math_grades >= 70 & class_grades$science_grades >= 80, c("students","group")]




##5-5. 리스트 만들기 --------------------------
list1 <- list(x=1, y=10:15, z=c(2, 9, 12))
list2 <- list(x2=c("Kim", "Park", "Lim", "Lee"))

str(list1)
str(list2)


# 리스트 결합, c() 이용
comb_list1 <- c(list1, list2)
str(comb_list1)


# 리스트 결합, list()함수 이용
comb_list2 <- list(list1, list2)
str(comb_list2)


# 몇 가지 유형을 혼합
list3 <- c(22, "세종", 15, FALSE, "부산", "TRUE")
str(list1) # 문자형으로 강제 변환


# list를 활용
list3 <- list(22, "세종", 15, FALSE, "부산", "TRUE")
str(list3)

# 리스트 안의 각 값(value)은 요소(element)

# 요소(elements)에 이름 부여
list4 <- list("Name" = "탕웨이",
              "Age" = 42,
              "Income" = 5000000,
              "Marriage" = TRUE)
str(list4)

# 이름으로 요소를 추출할 수 있음
list4$Name           # '$' 인덱스 태그 리스트
list4[["Name"]]
list4[[1]]
list4["Name"]


list4[["Income"]] + 5  # 값에 접근하여 계산 가능
list4["Income"] + 5    # 요소에 접근하기 때문에 계산 불가(콘솔 결과 참조)
list4[1]               # 요소에 접근
list4[c("Name","Income")]

# 하위 리스트를 가진 리스트도 가능
list5 <- list("Age" = 34,
              "height" = c(165, 180, 170),
              "sex" = "Female",
              "etc" = list(1, 3, 5, T, "T"))
str(list5)


list5[1]*3       # 계산 불가
list5[[1]]*3     # 계산 가능

list5[[4]][5]        # 네 번째 요소의 다섯번째 값
list5[["etc"]][[5]]  # 동일 결과

list5[["new"]] <- list5[["height"]] + 5
str(list5) # 리스트에 새로운 요소 추가된 것 확인


# `c()` 결합은 집으로 비유하자면, 같은 층에 거주하는 것
# `list()` 결합은 같은 집이긴 하지만, 층이 다른 곳에 거주하는 것을 의미





##5-6 배열(array)  --------------------------
# 배열(array)은 행렬과 유사하지만, 차원(dim=dimension) 속성을 가진 데이터 구조
# array(데이터, c(행개수, 열개수, 차원))

array(data = 1:20, c(5, 6, 3))

# 배열에서 데이터 선택
# x[행, 열, 차원]

x <- array(data = 1:20, c(5, 6, 3))
x
x[1, 2, 3]





#6. R 내장 데이터 불러오기  --------------------------
# R은 데이터 분석 연습을 위해 많은 내장 데이터를 제공


data()       # R 기본 내장 데이터 확인
data("iris") # "iris" datasets을 환경으로 로드
attach(iris) # 데이터 환경으로 부착

# 정보 확인
str(iris)
dim(iris) # 데이터셋의 행과 열 수만 출력
head(iris, 5) 
?iris   # iris 데이터 셋에 대한 일반 정보를 확인(변수 설명 등)

# save_date <- format(Sys.time(), "%y%m%d%H%M")
# write.csv(iris, paste0("iris_", save_date, ".csv"))

# 외에도 각 패키지에서 제공하는 패키지 내장 데이터도 존재
# ggplot2 패키지 내 예제 데이터 확인
data(package="ggplot2")

# 내장 데이터 객체로 저장
gg_data_list <- data(package="ggplot2")

# 객체 정보 확인
str(gg_data_list)
typeof(gg_data_list)
class(gg_data_list)
head(gg_data_list$results, 3)

data("diamonds",package="ggplot2")
diamonds

# 객체 정보 확인
str(diamonds)
typeof(diamonds)
class(diamonds)
head(diamonds, 6)


#7. R 외부 데이터 불러오기  --------------------------
# .csv file
# ""부분에 본인이 저장해 놓은 file 경로와 파일명을 입력
# iris_2208080118 <- read.csv("C:/Users/jangg/Dropbox/DellWorks/Doc/01_YHLim Projects/계명대 통계 워크샵_220809/R exercises/KMU_R_workshop/data/ch01_R basic exercises/iris_2208080118.csv", header = TRUE)


# 평균

Home_grades <- c(95, 72, NA, 79, 100, 82, 55)
# NA는 "not available" - 결측값(missing data)을 표시
is.na(Home_grades) # anyNA(Home_grades)
mean(Home_grades)
mean(Home_grades, na.rm = TRUE) # (na.rm = NA remove)

?mean

# 표준편차
sd(x = Home_grades, na.rm = TRUE)


#8. Examples of distributions --------------------------
rnorm(1) #난수 함수 - 정규분포함수의 변수값을 임의로 생성
rnorm(5)

x <- rnorm(10000)
hist(x)

x <- rnorm(10000, mean = 50, sd = 15) # 평균이 100, 표준편차 15인 정규분포 만들기
hist(x)

x <- runif(10000) # uniform distribution을 갖는 난수를 임의로 생성
hist(x)

x <- rexp(10000) # exponential distribution을 갖는 난수를 임의로 생성
hist(x)

qnorm(.05) # 입력은 확률(probability) - 분위수(a quantile)로 변함
pnorm(-1.96) # 입력은 랜덤 변수값 - 확률을 반환

qt(.95, df = 12) # 자유도 12인 t분포에서 누적확률이 0.95가 되는 확률변수 t값





#9. 벡터 작업 예제 -----------------

## 벡터 만들기
# 직접 입력
v1 <- c(10, 21, 13, 54, 15, 65, 4, 2, 11, 5)

# 수열(sequence)로 만들기
v2 <- 1:15
v3 <- seq(from = 1, to = 10, by = 0.5)
v4 <- seq(from = 1, to = 5, length = 3)
seq(from = 3, to = 12, length = 4)
seq_along(5:9)
seq_len(12)

# rep 함수 이용
v5 <- rep(0, 10)
v5

# rep 이용한 여러 값을 반복
v6 <- rep(c(1, 3, 5), each = 5)
v6
v7 <- rep(c(2, 4, 6), times = 5)
v7

# 벡터 결합
c(v6, v7) # same as
c(rep(c(1, 3, 5), each = 5), rep(c(2, 4, 6), times = 5))

## 벡터 연산
v8 <- v1 * 5
v8

v9 <- v6 + v7
v9


c(1, 2, 3, 4) + c(1, 2)
c(1, 2, 3, 4) + c(1, 2, 3) # error. 긴 객체의 길이가 짧은 것의 배수가 되어야 함


## 표집 함수(sample); 복원 표집(replace = TRUE)
v10 <- sample(x = v9, size = 30, replace = TRUE)
v10

set.seed(1) # 동일한 결과를 내기 위해 시드 설정. 숫자는 아무거나. 
v10 <- sample(v9, 30, replace = TRUE)
v10


## 인덱스로 값 추출과 대치
v9[1]
v9[5:10]
i <- c(1, 3, 5)
v9[i]
v9[c(1, 3, 5)] # 같은 결과

# 인덱스로 추출 순서 지정
v9[c(1, 2, 3, 4)]
v9[c(4, 3, 1, 2)]


# 인덱스된 값들만 추출
v9[c(1, 2, 3, 4, 1, 1, 1, 1, 1, 1)]


head(v9,  3)
tail(v9,  3)


v9[1]
v9[1] <- 100 # 값 대치(replace)
v9[c(2, 4, 6)] <- -555
v9


# 논리 연산자 사용
v9[v9 > 6]
mean(v9[v9 > 6])


which(science_grades > 80 & science_grades <= 90)




#10. 기술통계 분석

iris_prac <- iris # 연습 데이터 만들기
str(iris_prac)

# 요약 통계량
sum(iris_prac$Sepal.Length)     # 합계
mean(iris_prac$Sepal.Length)    # 평균
median(iris_prac$Sepal.Length)  # 중앙값
var(iris_prac$Sepal.Length)     # 변량
sd(iris_prac$Sepal.Length)      # 표준편차

# table 형태의 요약 통계계
summary(iris_prac)




#10. 결측치(NA; Not Available) 처리 -----------------

# 데이터 분석 전, 결측치 처리를 해야 함
# 직접 결측치 생성

class_grades_NAprac <- class_grades

class_grades_NAprac$math_grades[c(1,3)] <- NA

class_grades_NAprac # 첫 번째 세 번째에 결측값으로 대체됨

# 데이터가 크고, NA가 많지 않을 때 NA가 존재하는 행을 삭제
class_grades_NAprac <- na.omit(class_grades_NAprac)
class_grades_NAprac


# NA값 대체- 평균값
class_grades_NAprac_2 <- class_grades
class_grades_NAprac_2$science_grades[c(2,5)] <- NA
class_grades_NAprac_2

# 결측값의 수
sum(is.na(class_grades_NAprac_2))

# NA 확인법 2
complete.cases(class_grades_NAprac_2)

# NA가 있는 행 찾기: x[!comlete.cases(x),]
class_grades_NAprac_2[!complete.cases(class_grades_NAprac_2),]
class_grades_NAprac_2[is.na(class_grades_NAprac_2[3]),c(1:5)]  # 동일함

# 결측치를 평균으로 대체
# ifelse() 함수를 이용하여 NA값을 평균값으로 대체
# ifelse(조건, 그 조건을 만족할 때 실행되는 표현식, 조건을 불만족할 때 실행되는 표현식)
class_grades_NAprac_2$science_grades = ifelse(is.na(class_grades_NAprac_2$science_grades), 
                                              mean(class_grades_NAprac_2$science_grades, na.rm = T), 
                                              class_grades_NAprac_2$science_grades)





#11. 흐름 제어 ------------
# if (...) {
#   some R code
# } else {
#   more R code
# }


# if문
x = 1
y = 3

if (x > y) {
  z = x * y
  print("x 는 y보다 크다")
} else {
  z = x + 5 * y
  print("x 는 y보다 작거나 같다")
}

z

# ifelse문
ifelse(4 > 3, 1, 0)
ifelse(x > y, "와우", "저런")

testdt <- c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(testdt > 6, "와우", "저런")

# for문
x = 1:5
for (i in 1:5) {
  x[i] = x[i] * 2
}

x 

# for 응용: 구구단
for(i in 2:9){
  cat("---", i, "단---\n") # cat() 문자 표시 함수
  for(j in 1:9){
    cat(paste(i, "x", j, "=", i * j, "\n")) # paste() 문자 결합 함수
  }
}



#12. apply() 계열 함수 -----------------

# for문보다 apply계열 함수가 간략할 수 있음
# iris 데이터에서 컬럼을 하나씩 체크하여 
# numeric인 컬럼만 필터링한 데이터프레임 iris_num을 만드는 코드

# # for() 함수 예제
# iris_num <- NULL
# 
# for(i in 1:ncol(iris)){
#   if(is.numeric(iris[, i])) iris_num <- cbind(iris_num, iris[, i])
# }
# 
# iris_num <- data.frame(iris_num)
# 
# 
# # sapply 함수 예제
# iris_num <- iris[, sapply(iris, is.numeric)]


# 예제 데이터 만들기 

# iris 데이터 필터링
iris_num <- iris[1:10, 1:4]

# 랜덤으로 NA를 넣을 행/열 번호 뽑기
set.seed(123)
idx_r <- sample(1:10, 3) # 행 인덱스
idx_c <- sample(1:4, 3)  # 열 인덱스

# NA 넣기
for(i in 1:3){
  iris_num[idx_r[i], idx_c[i]] <- NA
}

iris_num


## apply()함수 ## 
apply(iris_num, 1, mean)   # 행(row) 단위 mean 연산
apply(iris_num, 2, mean)   # 열(column) 단위 mean 연산
# NA가 있는 행 또는 열은 mean 계산을 할 수 없음

# 열 단위, NA 제거하고 mean 연산
apply(iris_num, 2, mean, na.rm = T)
# 열 단위, 사용자 정의 함수(function) 연산
apply(iris_num, 2, function(x) { x * 2 + 1 })


## lapply 함수 ## 
# lapply 함수는 list + apply를 의미, 실행 결과가 list 형태로 출력

# iris_num의 열 단위 평균이 vector 형태로 출력
apply(iris_num, 2, mean, na.rm = T)

# iris_num의 열 단위 평균이 list 형태로 출력
lapply(iris_num, mean, na.rm = T)


## sapply 함수 ##
# sapply 함수는 연산 결과를 벡터 형태로 출력
sapply(iris_num, mean, na.rm = T)

# simplify = F이면 lapply와 동일하게 리스트 형태로 결과를 출력
sapply(iris_num, mean, na.rm = T, simplify = F)
lapply(iris_num, mean, na.rm = T)





#13. 기타 --------

# 객체 삭제 
# rm(x) 개별 객체 삭제
# rm(list=ls()) 메모리에 있는 전체 객체 삭제

test <- c(1, 2, 3, 3)
test

rm(test)

# working directory 확인
getwd()
setwd("")

# mannually working directory setting
# setwd("폴더 경로 삽입")
# 혹은 패널의 "File" 메뉴 중 "More"에서 설정 가능능