##### ------------------------------ ####
# date: 2022.08.09
# author: Forest Lim
# title: R basic
# R version 4.2.1 (2022-06-23 ucrt)/ # Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22000)
##### ------------------------------ ####

### package management ------------------------------

# Library path
.libPaths() 

# 설치 경로 변경, 사용자 패키지, 주로 4.1.1버전에 설치하였음
.libPaths("C:/Program Files/R/R-4.1.1/library")

# package import
library(dplyr)
library(ggplot2)


#1. data type
a <- 1
typeof(a) #

# R 내장 데이터 불러오기
data("iris")
attach(iris)

str(iris)
head(iris, 5) 


# central tendency


# Central Tendency


# The c() function
# 벡터를 만들때 매우 중요한 함수

a <- c(1,2,3,4,5)
class(a)
dim(a)

# atomic vectors can only contain elements of the same type

c(1, 2, "3")


#### exercises

#1. variable 
x1 <- c(4, 3, 2, 1, 5, 1) # 자부심
y1 <- c(5, 4, 2, 2, 4, 2) # 품위
z1 <- c(1, 2, 3, 4, 1, 1) # 심리적 붕괴

xyz <- data.frame(x1, y1, z1) # 데이터 프레임

