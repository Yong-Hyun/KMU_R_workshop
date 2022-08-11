##### ------------------------------ ####
# title: Correlation & Regression
# date: 2022.08.09
# author: Ph.D. Y-H Lim
# R version 4.2.1 (2022-06-23 ucrt)/ 
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22000)
##### ------------------------------ ####

### package management ------------------------------

# Library path
.libPaths() 

# 설치 경로 변경, 사용자 패키지, 주로 4.1.1버전에 설치하였음
.libPaths("C:/Program Files/R/R-4.1.1/library")

# package import
library(ggplot2)
library(dplyr)
library(ggstatsplot)

# 다수 패키지 한 번에 불러오기

# x <- c("ggplot2", "dplyr", "ggstatsplot")
# lapply(x, require, character.only = TRUE)




#1. 작은 데이터 상관분석 실습  --------
#### exercises

#1). variable 
x1 <- c(4, 3, 2, 1, 5, 1) # 자부심
y1 <- c(5, 4, 2, 2, 4, 2) # 품위
z1 <- c(1, 2, 3, 4, 1, 1) # 심리적 붕괴

xyz <- data.frame(x1, y1, z1) # 데이터 프레임

#2). plot
ggplot(xyz, aes(x=x1, y=y1))+
  geom_point()+
  geom_line()+
  xlab("자부심")+
  ylab("품위")

#3). add trend line
ggplot(xyz, aes(x=x1, y=y1))+
  geom_point()+
  geom_line()+
  geom_smooth(method = lm)+
  xlab("자부심")+
  ylab("품위")+
  theme(legend.position = "top", 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12)) 

#4). 상관계수 매트릭스 그리기

# pearson r 
ggcorrmat(xyz, type = "parametric", matrix.type="lower") 

# spearman rho 
xyz %>% 
  ggcorrmat(type="nonparametric" , matrix.type="lower") 



#2. 내장 데이터 이용 상관분석 실습 -------------

# 내장 데이터 객체로 저장
my_data <- mtcars

?mtcars   # 데이터 설명 자료
str(my_data) # 모두 수치형 자료
dim(my_data)
head(my_data, 6)




# 상관분석의 가정(assumptions)
# 1) 두 변수의 covariation이 선형적(linear)해야 함
# 2) 두 변수가 정규분포(normal distribution)해야 함

# 가정1) 
library("ggpubr") 

test <- ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


# 가정2) Shapiro-Wilk normality test
# mpg 데이터의 정규성 검정: 영가설-'데이터가 정규분포한다'
shapiro.test(my_data$mpg) # p = 0.1229 > 0.05; 영가설 수용
# wt 데이터의 정규성 검정
shapiro.test(my_data$wt) # p = 0.09>0.05; 영가설 수용 

# Q-Q 플롯(분위수 플롯)을 사용한 데이터 정규성의 시각적 검사. 
# Q-Q 플롯은 주어진 샘플과 정규 분포 사이의 상관 관계 보여줌
par(mfrow=c(1,2))
ggqqplot(my_data$mpg, ylab = "MPG")
# wt
ggqqplot(my_data$wt, ylab = "WT")

# 정규성 플롯(normality plots)을 통해, 두 모집단은 정규분포에서 기인함을 인지


# Pearson correlation test 
res <- cor.test(my_data$wt, my_data$mpg, method = "pearson")  # stats 패키지에서
res

# Kendall 순위 상관 계수(Kendall의 타우 통계)는 순위 기반 측정치의 관련성을 추정
# 이변량 정규 분포에서 나온 것이 아닌 경우 사용
res2 <- cor.test(my_data$wt, my_data$mpg,  method="kendall")
res2

# Spearman의 rho 통계량 또한 순위 기반 측정치의 관련성을 추정
# 이변량 정규 분포에서 나온 것이 아닌 경우 사용
res3 <-cor.test(my_data$wt, my_data$mpg,  method = "spearman")
res3


# 상관행렬
# Load data
data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
head(my_data, 6)

# 상관계수
rescm <- cor(my_data)
round(rescm, 2) # 반올림

# 공분산
cov(my_data) 


# 상관관계의 유의수준
library("Hmisc")
resH <- rcorr(as.matrix(my_data))  #rcorr() 피어슨 및 스피어맨 상관 관계에 대한 유의 수준을 계산
resH


# 함수를  상관[참조]계수 매
# symbols: 사용할 기호
# abbr.colnames: 논리적 값. TRUE이면 colname이 축약

symnum(rescm, abbr.colnames = FALSE)


# corrplot() 함수 사용: 상관도 그리기
library(corrplot)
corrplot(rescm, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# 양의 상관관계는 파란색, 음의 상관관계는 빨간색
# 색상 강도와 원의 크기는 상관 계수에 비례
# 상관도의 오른쪽 범례 색상은 상관 계수의 해당 색상

# 상관도 유의성 검정
#Insignificant correlation are crossed
corrplot(resH$r, type="upper", order="hclust",p.mat = resH$P, sig.level = 0.05, insig = "blank")

# Insignificant correlations are leaved blank
corrplot(resH$r, type="upper", order="hclust", p.mat = resH$P, sig.level = 0.01, insig = "blank")


library("PerformanceAnalytics")
# chart.Correlation()함수는 상관 행렬의 차트를 표시하는 데 사용
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# 각 변수의 분포는 대각선에 표시
# 대각선 하단: 적합선이 있는 이변량 산점도가 표시
# 대각선 상단: 상관 값+ 별 유의 수준





#3. 부분상관(partial) 준부분(semi-partial 또는 part) 상관 분석  --------
# 용어 혼란: 편상관, 준편상관/여과상관, 준여과상관

library(ppcor) 

#### 부분상관(partial correlation) 
# IV: x1, x2, DV: y가 있을 때 x2의 효과를 제거 후 x1이 **남아있는 y의 변량(전체변량 아님)**을 설명하는 비율
# - 결국 x1과 y의 상관관계(x1을 통제했을 때도 마찬가지)
# - 부분상관이 원래 상관보다 더 오르면? x2가 억제변인으로 작용하던 것이고
# - 더 낮아지면? x2가 허위변인으로 작용하던 것.

dataset <- iris
str(dataset)
dataset$Species <- NULL  # 상관분석에 요인변수는 없애야 함. 
pcor(dataset, method="pearson")               # partial correlation 콘솔에서 한 번 확인
iris_pcor <- pcor(dataset, method="pearson")   # partial correlation 결과를 객체 할당
iris_pcor$estimate # partial correlations만 추출할 수 있음
iris_pcor$p.value # results of significance tests만 추출
iris_pcor$statistic # t-test만 추출

# partial correlation between x and y while controlling for z
partial_cor <- pcor.test(dataset$Sepal.Length, dataset$Petal.Length,
                         dataset$Sepal.Width,
                        method = "pearson")
partial_cor$estimate  #각각 결과를 추출할 때
partial_cor$p.value
partial_cor$statistic



#### 준부분상관(semi-partial correlation): 변수의 고유한 영향력
# IV: x1, x2, DV: y가 있을 때 x2의 효과를 제거 후 x1이 **y의 전체 변량**을 설명하는 비율
# x1의 독자적인 영향력을 볼 수 있음

iris_spcor <- spcor(dataset, method = "pearson")
iris_spcor$estimate  #각각 결과를 추출할 때
iris_spcor$p.value   #각각 결과를 추출할 때
iris_spcor$statistic #각각 결과를 추출할 때


partCor <- spcor.test(dataset$Sepal.Length, dataset$Petal.Length,
                      dataset$Sepal.Width,
                      method = "pearson")

partCor$estimate
partCor$p.value
partCor$statistic

# 참조:
#  - 다중 회귀에서, 다음은 검사해야 하는 준부분(또는 부분) 상관관계: 
#  - X1, X2, X3를 예측 변수로, Y를 기준으로 하는 모델을 가정. 
#  - Y에서 X2와 X3를 제거한 후 X1과 Y의 준부분 상관이 필요. 
#  - 1단계에서 Y ~ X2 + X3 중다회귀를 수행. 
#  - 2단계에서 Y의 잔차를 RY. 
#  - 3단계에서 RY ~ X1을 회귀분석함
#  - 3단계에서 얻은 상관 계수는 찾고 있는 준부분 상관





#4. 단순 회귀분석 -----------

# lm()는 선형 회귀 모델을 계산하는 데 사용
# 회귀 방정식 "sales = b0 + b1*youtube"
marketing=data.frame(sales=c(2,4,6,9,12,34,45),youtube=c(1,2,4,7,9,11,15))
model <- lm(sales ~ youtube, data = marketing)
summary(model)$coef

# 모델을 이용한 예측, 유튜브 광고가 0인 경우와 1000인 경우의 데이터 넣기기
newdata <- data.frame(youtube = c(0,  1000))
model %>% predict(newdata)
# YouTube 광고 예산이 0인 경우 -5.3636364 단위의 판매를 기대
# 광고 예산이 1000인 경우 -5.363636 + 3.051948*1000 = 3046.584 단위의 판매를 기대할 수 있음

# 모델의 통계적 유의성 검정
summary(model)

###출력에는 다음을 포함한 6개의 구성요소가 표시됨###

# Call: 회귀 모델을 계산하는 데 사용되는 함수 호출을 표시
# 잔차(Residuals): 정의에 따라 평균이 0인 잔차 분포에 대한 보기를 제공. 중앙값은 0에서 멀지 않아야 하며 최소값과 최대값은 절대값으로 거의 같아야 함
# 계수(Coefficients). 회귀 베타 계수와 통계적 유의성 표시. 결과 변수와 유의하게 연관된 예측 변수는 별표로 표시
# 잔차 표준 오차(Residual standard error;RSE), R-제곱(R2) 및 F-통계량은 모델이 데이터에 얼마나 잘 맞는지 확인하는 데 사용
# F-통계의 p-값이 0.002로 매우 유의. 이것은 적어도 예측 변수 중 하나가 결과 변수와 유의하게 관련되어 있음을 의미

#계수 유의성: 어떤 예측 변수가 유의한지 확인하기 위해 회귀 베타 계수의 추정값과 관련 t-통계량 p-값을 보여주는 계수 표
summary(model)$coef
# 결과-유튜브 광고예산의 변화는 매출의 변화와 큰 연관이 있음

# 선형 회귀는 결과와 예측 변수 간의 선형 관계를 가정. 
# 이는 결과 변수 대 예측 변수의 산점도를 만들어 쉽게 확인
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()

# 회귀 진단(Regression diagnostics)
par(mfrow = c(2, 2))
plot(model)

### 결과 해석###
# 잔차 대 적합Residuals vs Fitted. 선형 관계 가정을 확인하는 데 사용. 뚜렷한 패턴이 없는 수평선은 선형 관계, 즉 좋은 것을 나타냄. 이 사례에서는 그렇지 않음. 

# 노멀 QQ(Normal Q-Q): 잔차가 정규 분포를 따르는지 여부를 조사. 잔차 점이 직선 파선을 따르는 것이 좋음. 우리 사례에서는 그렇지 않음.

# Scale-Location (or Spread-Location). 잔차 분산의 동질성을 확인(등분산성). 균등하게 퍼진 점이 있는 수평선은 등분산성을 잘 나타냄. 우리 사례는 이분산성 문제가 있음.

# Residuals vs Leverage: 분석에 포함되거나 제외될 때 회귀 결과에 영향을 미칠 수 있는 이상치의 사례를 식별하는 데 사용, 또한
# 위의 플롯 4는 0.5 이상의 표준화된 잔차와 함께 가장 극단적인 상위 2개 지점(#10 및 #7)을 강조 표시,
# 그러나 3 표준 편차를 초과하는 이상치는 없음.

# 추정된 회귀방정식: sales = -5.3636364 + 3.0519481* youtube




library(effectsize)   # for parameters_standardize
library(parameters)   # for model_parameters
library(performance)  # for model_performance etc..
library(ggeffects)    # for plotting models

#5. 중다회귀분석(Multiple Linear Regression) -----

# # install.packages("pacman")
# pacman:: p_load("tidyverse", "ggplot2", "ggpubr", "datarium")
# data("marketing", package = "datarium")
# mydata <- marketing
# attach(mydata)
# 
# #check the corelation between the predoctprs and the outcome
# # install.packages("Hmisc")
# 
# # 패키지 설치(수동)
# if(!require("Hmisc")){
#  install.packages(c("Hmisc"))}
# 
# library(ggeffects)    # for plotting models
li

# 데이터셋 로드하기
data(hardlyworking, package = "effectsize")
head(hardlyworking)
# - salary      : (한달 급여; 이스라엘 통화단위 Shekels per month)
# - xtra_hours  : 주당 초과 근무 시간
# - n_comps     : 상사에 제공되는 칭찬의 수
# - age         : 나이.
# - seniority   : 근속년수


##5-1) 단순 회귀 예제 ----

# R에서 모델은 두 부분으로 구축
# 1. 모델 지정(*Specifying* the model)
#   - 우리가 추정하고 싶은 매개변수들은 무엇인가?
# 2. 어떤 데이터를 모델에 적합(*Fitting* the model to some data)
#   - 실제로 어떤 데이터를 사용해 매개변수를 추정함


# 모델들은 보통 어떤 식(formula)으로 지정됨
y ~ x
# y는 x의 함수이다로 읽음. 혹은 y는 x에 의해 예측됨


# 선형 모델은 `lm()` 사용
fit <- lm(salary ~ xtra_hours, data = hardlyworking)
fit

# 이젠 모델을 갖게 됐고, 우리는 평가하고 탐색할 수 있음


# 기본적인 작업: estimate, SE, test values 등등...
summary(fit)


## 모델의 매개변수들을 탐색

# CIs
confint(fit)

# beta
standardize_parameters(fit, method = "basic")

# 한 번에 보기 
model_parameters(fit)
model_parameters(fit, standardize = "basic")



## 모델 평가
# 모델 인덱스 살펴볼 것
rmse(fit)   # or mae(fit)
r2(fit)     # 등등
model_performance(fit)



## 예측(Prediction) 
# 그리고 잔차(residuals)
predict(fit)
residuals(fit)


# 새로운 데이터로 예측할 수도 있음
(new_observations <- data.frame(xtra_hours = c(-15, 30)))
predict(fit, newdata = new_observations)


## Plot 
gge_xtra_hours <- ggpredict(fit, "xtra_hours")
gge_xtra_hours
plot(gge_xtra_hours)
plot(gge_xtra_hours, add.data = TRUE, jitter = 0)

# 참조: https://strengejacke.github.io/ggeffects






##5-2) 중다 회귀분석 예제  ----
# 식 안에 중다 예측변수들은 "+" 로 결합됨
fit2 <- lm(salary ~ xtra_hours + n_comps, data = hardlyworking)

summary(fit2)


## 모델 매개변수 탐색 
model_parameters(fit2)
model_parameters(fit2, standardize = "basic") # Get Betas


# 이것이 결과에 미치는 영향은?
hardlyworking$xtra_minutes <- hardlyworking$xtra_hours * 60

fit3 <- lm(salary ~ xtra_minutes + n_comps, data = hardlyworking)

model_parameters(fit2)
model_parameters(fit3)



## 모델 평가
model_performance(fit2)


## Predict
new_obs2 <- data.frame(xtra_hours = c(0, 5),
                       # What are negative compliments??
                       # What is HALF a compliment??
                       n_comps = c(1.5, -2))
new_obs2
predict(fit2, newdata = new_obs2)


## Plot
ggpredict(fit2, "xtra_hours")               |> plot(add.data = TRUE, jitter = 0)
ggpredict(fit2, "n_comps")                  |> plot(add.data = TRUE, jitter = 0.1) 
ggpredict(fit2, c("xtra_hours", "n_comps")) |> plot(add.data = TRUE, jitter = 0)


# 중다 회귀에서 raw data 대신 부분잔차(partial residuals) 사용하고 싶다면, 
# `residuals = TRUE` 를 설정. 
# 추가 정보:  https://strengejacke.github.io/ggeffects/articles/introduction_partial_residuals.html
































