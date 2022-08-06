#1 히스토그램 만들기
BMI<-rnorm(n=1000, m=24.2, sd=2.2) # 평균과 표준편차, 사례수를 이용하여 정규분포 만들기
hist(BMI) # 히스토그램 그려보기

#### 급간을 세부적으로 만들어 보기 ####
# 1. 먼저 히스토그램을 새변수에 저장
histinfo <- hist(BMI)
histinfo # 히스토그램함수가 디폴트 값으로 어떻게 급간을 나누었지 확인

# 2. 급간(bin)의 수를 조정하기
hist(BMI, breaks=20, main="Breaks=20")
hist(BMI, breaks=5, main="Breaks=5")
# 하지만 결과값을 보면 내가 설정한 값과 일치하지 않는다. 
# 그것은 R 의 알고리즘이 대체로 내가 원하는 정도에 근접하게 데이터를 자르기 때문.


# 3. 정확하게 급간을 설정하기
hist(BMI, breaks=c(17,20,23,26,29,32), main="Breaks is vector of breakpoints")
## 급간을 벡터로 설정한 결과이다. 
## 즉 This dictates exactly the start and end point of each bin. 오케바리?


# 4. 수열을 통해 코드를 깔끔하게 만들기
hist(BMI, breaks=seq(17,32,by=3), main="Breaks is vector of breakpoints")


#### 세로축을 빈도(frequency)가 아니라 밀도(density)로 만들기 ####
hist(BMI, freq=FALSE, main="Density plot")
## y축을 확인해 보라. 

# 이제 히스토그램의 밀도가 정확하게 나타나는지 확인하자. 
# 그러기 위해선 plot=FALSE 로 설정하고, 밀도의 합이 1이 나오는지 확인하자.
# 왜냐하면 우리는 히스토그램 자체에 관심이 있는 것이 아니라 밀도값에 관심이 있기 때문.
hist1 <- hist(BMI, plot=FALSE)
hist1$density # hist1변수의 밀도값을 반환하라.
# 이제 밀도의 합을 살펴보자. 1이 나오면 정상.
sum(hist1$density) #hist1의 밀도를 합산하라.







