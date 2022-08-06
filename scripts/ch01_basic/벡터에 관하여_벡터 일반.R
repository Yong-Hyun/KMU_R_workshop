######## 벡터(Vector) ########

### 기본함수 ###
# 벡터는 R에서 가장 단순한 객체로서 여러 값들이 일차원적으로 순서화된 것으로 
# 벡터의 일부분만을 추출하여 이용하는 것을 용이하게 해준다. 
# 벡터를 생성하기 위해서는 일반적으로 c() 함수를 사용하여 
# 각 값들을 묶어서 하나의 행을 만든다.

# 예제 

v1 <- c(1,2,3)         # 숫자형 벡터 생성
v1
# [1] 1 2 3

v2 <- c("a", "b", "c")         # 문자형 벡터 생성
people <- c("Ned", "Jill", "Pat") 
# [1] "a" "b" "c"

v3 <- c(T, F, T)          # 논리형 벡터 생성
v3
# [1] TRUE FALSE TRUE

height <- c(160, 140, 155)         # height 벡터 생성 
people <- c("Ned", "Jill", "Pat")

names(height) <- people         # height 벡터의 원소에 이름을 할당
height
# Ned  Jill   Pat 
# 160   140   155 

height["Ned"]               # 첫 번째 벡터만 출력
# Ned 
# 160 

names(height) <- NULL         # height 벡터의 원소의 이름을 삭제 
height
# [1] 160 140 155



### 관련함수 ###
# 벡터에서 자료를 삽입하고 삭제와 관련된 다양한 함수들을 살펴보자. 
# 이러한 함수들에는 replace(), append(), order(), sort() 함수 등이 있다.

# 예제 

vec1 <- c(1,2,3,4,5)        # 1~5까지 자료를 갖는 vec1 변수 생성
vec1[2]        # 두 번째 자료
# [1] 2

vec1[c(2,3,5)]         # vec1의 2, 3, 5의 값만 표현
# [1] 2 3 5
vec1[c(-2,-3)]        # vec1의 2, 3번째 자료 값 삭제(완전 삭제가 아닌 삭제해서 표시하는 기능)
# [1] 1 4 5
vec1[vec1>2]        # vec1에서 2보다 큰 값만 표현
# [1] 3 4 5
vec1[2] <- 6         # 두 번째 위치의 2값이 6으로 대체됨
vec1
# [1] 1 6 3 4 5

replace(vec1, 3, 2)         # vec1의 세 번째 자료를 2로 변경
# [1] 1 6 2 4 5

append(vec1, 8, 5)         # vec1의 5번째 자료 다음에 8을 삽입
# [1] 1 6 3 4 5 8

v1 <- 1:3
v2 <- 2:4
v1 * v2        # 벡터의 각 원소간 곱셈
# [1] 2 6 12


# 예제2 

height <- 175         # height 스칼라 생성
heights <- c(160,140,155)         # heights 벡터 생성
heights[c(2,1,2)]         # heights의 2, 1, 2번째 원소 추출
# [1] 140 160 140

heights <- append(heights, height)         # heights와 height를 결합
heights 
# [1] 160 140 155 175

heights.1 <- append(heights,180,after=2)          # heights의 2번째 다음에 180 추가
heights.1
# [1] 160 140 180 155 175

heights <- replace(heights, 2,142)          # heights의 2번째 원소를 142로 변경
heights
# [1] 160 142 155 175

heights.2 <- replace(heights,c(2,4),c(140,142))         # 2번째 140, 4번째 142로 변경
heights.2
# [1] 160 140 155 142

numbers <- 1:5         # 1~5의 값을 갖는 벡터 생성 
numbers
# [1] 1 2 3 4 5 

heights <- heights.2[2:4]          # heights.2에서 2~4번째 값까지만 생성 
heights
# [1] 140 155 142

length(heights)         # 벡터의 길이 지정 
# [1] 3

x <- c(rep(1,3),seq(1,5,by=2),rev(seq(1,5,length=3)),rep(2,3))
x
# [1] 1 1 1 1 3 5 5 3 1 2 2 2

unique(x)          # 반복없이 벡터의 원소 표시
# [1] 1 3 5 2 

sort(x)          # 오름차순 정렬 
# [1] 1 1 1 1 1 2 2 2 3 3 5 5

rev(sort(x))          # 내림차순 정렬 
# [1] 5 5 3 3 2 2 2 1 1 1 1 1

rank(x)          # 자료의 순위를 지정
# [1] 3.0  3.0  3.0  3.0  9.5  11.5  11.5  9.5  3.0  7.0  7.0   7.0

order(x)          # 오름차순에 의한 자료의 위치값
# [1] 1 2 3 4 9 10 11 12 5 8 6 7

rle(x)
# Run Length Encoding 
# lengths: int [1:6] 4 1 2 1 1 3          # 자료가 반복되는 길이 
# values : num [1:6] 1 3 5 3 1 2          # 반복되는 자료의 값