##### ------------------------------ ####
# my session informations

# title: Data Wrangling; dplyr 패키지를 이용한 데이터 정제
# date: 2022.08.09
# author: Ph.D. Y-H Lim
# R version 4.2.1 (2022-06-23 ucrt)/ # Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22000)

# package management -----------------------

# Library path 확인
.libPaths() 

# 설치 경로 변경, 사용자 패키지, 주로 4.1.1버전에 설치하였음
# .libPaths("C:/Program Files/R/R-4.1.1/library") # 이건 실행되지 않게 하세요. 


##### ------------------------------ ####



# 데이터 정제, 즉 전처리 과정을 실습합니다. 
# 연구를 수행할 때 데이터 수집-코딩-분석-결과기술의 단계가 있다면, 
# 데이터를 전처리하는 과정에서 가장 많은 시간이 소요됩니다. 
# 그 과정을 단축할 수 있는 방법을 실습해 봅니다. 
# 또한 엄청나게 큰 규모의 데이터도 조작할 수 있는 방법을 배웁니다. 




#1. 패키지(packages) 설치 및 로드 --------------------------


# 패키지 설치(수동)
if(!require("dplyr")){
  install.packages(c("dplyr"))
}

if(!require("ggplot2")){
  install.packages(c("ggplot2"))
}


# 패키지 하나씩 로드
library(dplyr)
library(ggplot2)

# 여러 패키지 한 번에 로드(필요할 때 아래 주석을 해제)
# pkg <- c("dplyr", "ggplot2")
# lapply(pkg, require, character.only = TRUE)

search() # 로드된 패키지 확인
ls(package:dplyr) # 패키지 내 사용할 수 있는 함수목록
length(ls(package:dplyr)) # 함수의 갯수




#2. 대용량 데이터 로드 --------------------------
if(!require("hflights")){
  install.packages(c("hflights"))
}

search() # 비행데이터 로딩되었는지 확인

data("hflights") # "hflights" datasets을 환경으로 로드
attach(hflights) # 데이터 환경으로 부착

# 정보 확인
str(hflights)
dim(hflights) # 데이터셋의 행과 열 수만 출력
head(hflights, 5) 
?hflights   # hflights 데이터 셋에 대한 일반 정보를 확인(변수 설명 등)




#3. dplyr 패키지 - filter() 행 추출 --------------------------

# * filter()는 조건에 따라 행(row)을 추출
# * 사용법: 
#   + 첫 번째 인수: 데이터 프레임을 지정
#   + 두 번째 인수: 행의 조건을 지정 
#   + AND 조건문 콤마(,)로 구별하거나 & 연산자를 사용
#   + OR 조건문은 | 연산자를 이용


# 연습을 위해 새로운 객체에 데이터 저장
flights <- hflights

# 1월 1일 데이터만 추출
Jan01 <- filter(flights, Month == 1, DayofMonth == 1)

dim(Jan01) # 데이터 차원 확인, 행만 축소되고, 변인(칼럼)은 그대로 유지

head(Jan01)
tail(Jan01)

# 3월, 5월, 7월 데이터 추출
MarMayJul <- filter(flights, Month == 3 | Month == 5 | Month == 7)

dim(MarMayJul) # 데이터 차원 확인

head(MarMayJul)
tail(MarMayJul)





#4. dplyr 패키지 - select() 열 추출 --------------------------


# * 함수 select()는 열(column) 추출
# * 복수의 열을 추출할 때에는 콤마(,)로 구분 
# * 인접한 열을 추출할 때에는 : 연산자를 이용
# * 열 제외
# + 괄호 안에 제외하고 하고 싶은 열의 이름을 지정하고 괄호 앞에 -(minus) 부호를 붙임


# Year, Month, ArrDelay 열을 추출
delay <- select(flights, Year, Month, ArrDelay)

dim(delay) # 데이터 양 확인

head(delay, 3) # 데이터 앞부분 3개만 콘솔에 출력
tail(delay)    # 데이터 뒷부분 콘솔에 출력

rm(delay) # 데이터 양이 커서 그냥 삭제


# 인접한 열 추출- " : " 기호 사용

# Month부터 DayOfWeek까지 추출(Year, Month, DayofMonth, DayofWeek)
extCol_01 <- select(flights, Month:DayOfWeek)

dim(extCol_01)

# Month부터 DayOfWeek를 제외한 나머지 열을 추출
extCol_02 <- select(flights, -(Month:DayOfWeek))

dim(extCol_02)

rm(extCol_01, extCol_02) # 데이터 양이 커서 그냥 삭제





#5. dplyr 패키지 - mutate() 새로운 변수 생성 --------------------------

# * 계산 등을 이용하여 열을 추가할 때 사용
# * mutate()는 함수에서 새로 만든 열을 같은 함수 안에서 바로 사용가능한 장점


# 함수 내에서 생성한 열을 다음 계산에 사용가능
# gain을 gain_per_hour의 계산에 사용할 수 있음
gain_per_hour <- mutate(flights, gain = ArrDelay - DepDelay, gain_per_hour = gain/(AirTime/60))

dim(gain_per_hour)  # 23개 변수로 증가
head(gain_per_hour, 3)



#6. dplyr 패키지 - summarise() 데이터 요약 --------------------------

# * data 요약을 할 수 있음
# * mean(), sd(), var(), median() 등의 함수를 지정하여 기초 통계량 집계
# * 결과는 데이터 프레임 형식


# 평균 출발지연시간 계산
fly_mean <- summarise(flights, delay = mean(DepDelay, na.rm = TRUE)) # na.rm = TRUE 결측값을 제외 옵션

str(fly_mean)
dim(fly_mean)


# 여러 변수의 평균법_1; 직접 기입 후 새 객체에 저장


dep_arr_mean <- flights %>%
  summarise(DepTime_mean = mean(DepTime, na.rm = TRUE),
            ArrTime_mean = mean(ArrTime, na.rm = TRUE))
# %>% (pipline) 앞의 계산 결과를 뒤이어 상속. 사고의 흐름대로 분석을 수행하게 해줌


# 여러 변수의 평균법_2; 수치형 변수들은 한 번에 평균을 구함. 결측값은 제외 후 평균 구함
avg <- summarise(flights, across(where(is.numeric), mean, na.rm = TRUE))

avg
dim(avg)





#7. dplyr 패키지 - arrange() 데이터 정렬 --------------------------

# * 함수 arrange()는 지정한 열을 기준으로 작은 값으로부터 큰 값의 순으로 데이터를 정렬
# * 역순 정렬은 함수 desc()를 함께 사용


# 데이터를 ArrDelay, Month, Year 순으로 정렬
sort_flights <- arrange(flights, ArrDelay, Month, Year)

dim(sort_flights)
str(sort_flights)
head(sort_flights)



#8. dplyr 패키지 - group_by()를 이용한 그룹화 --------------------------

# * 함수 group_by()를 이용하면 지정한 열의 수준(level)별로 결과를 제공
# * 비행편수 50편 이상, 평균 비행 거리 1,500마일 이상인 항공사별 평균 연착시간을 계산하여 그림으로 표현
planes <- group_by(flights, FlightNum)
dim(sort_flights)


delay <- summarise(planes, count = n(),                     # n() 빈도수
                   dist = mean(Distance, na.rm = TRUE), 
                   delay = mean(ArrDelay, na.rm = TRUE))
dim(delay)

# filter로 조건에 맞는 케이스만 추출
delay <- filter(delay, count > 50, dist < 1500)
dim(delay) 




 
#9. 정제한 데이터 시각화 - ggplot2 이용 --------------------------

library(ggplot2)

# Plots 패널에 표시됨
ggplot(delay, aes(x = dist, y = delay)) +            # 데이터 지정
  geom_point(aes(size = count), alpha = 0.4) +       # 점 속성
  geom_smooth(method ='lm')                          # 선형 추세선





#10. factor 변수(범주변수)의 처리 방법 --------------------------

#### data 생성, data.frame()
  
# 1500명의 가상 데이터를 생성

set.seed(123)   # 동일 결과 재생을 위해 시드 설정. 숫자는 아무거나. 

mydata <-
  data.frame(
    subjectID  = seq(1, 1500, by=1),
    sex  = sample(2, 1500, replace = TRUE),
    incomeLevel = sample(c("High", "Middle", "Low"), 1500, replace = T),
    parenEduLevel = sample(c("High", "MiddleHigh", "MiddleLow", "Low"), 1500, replace = T),
    cal_1dy  = sample(700:2500, 1500, replace = TRUE),
    height  = as.double(rnorm(1500, mean = 175, sd = 50)),
    weight  = as.double(rnorm(1500, mean = 65, sd = 22)),
    anxiety = as.double(rnorm(1500, mean = 17, sd = 12)),
    depress = sample(1:20, 1500, replace = T)
  ) 

# 객체 정보 확인
class(mydata) 
str(mydata) # 객체내 변수들의 유형 확인
dim(mydata)
head(mydata)
tail(mydata)



# 데이터 요약
summary(mydata)


# 성별 변수를 factor 로 변환해야 함. 
levels(mydata$sex)            # NULL, 아직 수준이 없음
levels(mydata$imcomeLevel)    # NULL, 아직 수준이 없음
levels(mydata$parenEduLevel)  # NULL, 아직 수준이 없음


# * sex - 정수, 
# * 소득수준(incomeLevel) - 요인, 
# * 부모교육수준(parent Edu Level) - 요인, 그외 수치형

# 캐릭터 변수(sex, incomeLevel, parenEduLevel)를 팩터로 변경
# * 정수형으로 코딩된 성별을 팩터로 변환
# * 1은 남자(M), 2는 여자로 다시 코딩
# * `factor(x, level, labels)`
#   + x = 요인으로 변경할 자료
#   + level = 요인화될 자료가 가지는 값
#   + labels = 실제 보여질 값

mydata$sex <- factor(mydata$sex, levels = c(1, 2), labels = c("M", "F"))
levels(mydata$sex)
str(mydata$sex)
class(mydata$sex)

mydata$incomeLevel <- factor(mydata$incomeLevel, levels =  c("High", "Middle", "Low"))
levels(mydata$incomeLevel)
str(mydata$incomeLevel)
class(mydata$incomeLevel)

mydata$parenEduLevel <- factor(mydata$parenEduLevel, levels = c("High", "MiddleHigh", "MiddleLow", "Low"))
levels(mydata$parenEduLevel)
str(mydata$parenEduLevel)
class(mydata$parenEduLevel)

# level 순서 변경
levels(mydata$sex) <- c("F", "M")
levels(mydata$sex)



#### factor 정리

# * 심리학 연구를 하며 가장 중요하게 다루는 변수가 바로 factor 변수
# * factor를 다루는 패키지`forcats package`를 활용할 수도 있음
# * 팩터를 잘 다루면, 데이터 분석이 한결 쉽고 의미를 쉽게 발견견





#11. 'mydata' variable frequency --------------------------

str(mydata)

library(tidyr) #drop_na() 함수 위해. 

# 빈도수 구하기
mydata_n <- mydata %>% 
  drop_na() %>% # 결측값 제외, 
  count(sex, incomeLevel, parenEduLevel)

str(mydata_n) # n 칼럼이 추가된 걸 확인할 수 있음

# sex 빈도수 구하기
mydata_n_sex <- mydata %>% 
  drop_na() %>% 
  count(sex)

str(mydata_n_sex)

# incomeLevel 빈도수 구하기
mydata_n_incom <- mydata %>% 
  drop_na() %>% 
  count(incomeLevel)

str(mydata_n_incom)


# parenEduLevel 빈도수 구하기
mydata_n_pare <- mydata %>% 
  drop_na() %>% 
  count(parenEduLevel)

str(mydata_n_pare)





#12. 평균과 테이블화 --------------------------

# 소득수준별 평균구하기
mydata_inc_mean <- mydata %>%
  group_by(incomeLevel) %>%                # 팩터별 집계
  summarise(cal_mean = mean(cal_1dy),
            height_mean = mean(height),
            weight_mean = mean(weight),
            anxiety_mean = mean(anxiety),
            depress_mean = mean(depress))

# 교육수준별 평균구하기
mydata__par_mean <- mydata %>%
  group_by(parenEduLevel) %>%
  summarise(cal_mean = mean(cal_1dy),
            height_mean = mean(height),
            weight_mean = mean(weight),
            anxiety_mean = mean(anxiety),
            depress_mean = mean(depress))

# 소득수준과 교육수준을 동시에 고려한 평균구하기
mydata__inc_par_mean <- mydata %>%
  group_by(incomeLevel, parenEduLevel) %>%
  summarise(cal_mean = mean(cal_1dy),
            height_mean = mean(height),
            weight_mean = mean(weight),
            anxiety_mean = mean(anxiety),
            depress_mean = mean(depress))



