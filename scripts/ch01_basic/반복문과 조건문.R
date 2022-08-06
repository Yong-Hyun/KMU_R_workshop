#### 반복문과 조건문 ####

# 1. if 문 사용

x <- c(1,2,3) # 숫자 벡터를 만들었다.
x <- factor(x) #숫자 벡터 x를 요인으로 재구성
if(is.factor(x)) length(x)

# - is.factor(x) 는 x 가 요인일 경우 TRUE를 그렇지 않으면 FALSE 반환.
# - x 가 요인이므로 TRUE
# - if 문에서 TRUE 일 경우 length(x)를 실행한다. 이 경우 TRUE 이므로 length(x) 실행, FALSE 일 경우 실행하지 않고 다음으로 이동
# - length(x)의 실행값인 3 출력


# 2. if else 문 사용

if(is.factor(x)) { 
  length(x)     
} else { # x가 요
    sum(x)
}

# line 17 : x가 요인이면
# line 18 : x의 끝까지 반환하고
# line 19 : x가 요인이 아니면
# line 20 : x를 합산하라.

# 3. if else if : 첫 번째 조건이 False 일 경우 else이후 조건문 추가

if(is.factor(x)) {
    length(x)
} else if(is.integer(x)) {
    sum(x)
} else {
    paste(x, "element")
}