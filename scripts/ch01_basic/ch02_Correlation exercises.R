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
library(ggplot2)
library(dplyr)
library(ggstatsplot)

# 다수 패키지 한 번에 불러오기

# x <- c("ggplot2", "dplyr", "ggstatsplot")
# lapply(x, require, character.only = TRUE)


#### exercises

#1. variable 
x1 <- c(4, 3, 2, 1, 5, 1) # 자부심
y1 <- c(5, 4, 2, 2, 4, 2) # 품위
z1 <- c(1, 2, 3, 4, 1, 1) # 심리적 붕괴

xyz <- data.frame(x1, y1, z1) # 데이터 프레임

#2. plot
ggplot(xyz, aes(x=x1, y=y1))+
  geom_point()+
  geom_line()+
  xlab("자부심")+
  ylab("품위")

#3. add trend line
ggplot(xyz, aes(x=x1, y=y1))+
  geom_point()+
  geom_line()+
  geom_smooth(method = lm)+
  xlab("자부심")+
  ylab("품위")+
  theme(legend.position = "top", 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12)) 

#4. plot of the correlation coefficient matrix 
ggcorrmat(xyz, corr.method = "pearson", matrix.type="lower") 
