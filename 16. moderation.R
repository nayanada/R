# Statistics One, 2013, Lab 7

# Lab goals
#   Conduct moderation analysis

#   Moderation analysis
#     Example
#     An experimental research investigation of the effects of stereotype threat on intelligence testing 
#       Dependent variable (Y) is score on an intelligence test (IQ)
#       Independent variable (X) is the treatment condition (3 levels: control, explicit threat, implicit threat)
#       Moderator variable is score on a working memory task
#       Sample size of N = 150 (n = 50)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("multilevel")

# Load packages
library(psych)
library(ggplot2)
library(multilevel)

# Read data into a dataframe called MOD
MOD <- read.table("stats1-datafiles-Stats1.13.Lab.07.txt", header = T)

# Data 둘러보기
head(MOD, 8)

# D1, D1 살펴보기 (D1, D2 생성에 대해서는 GLM에서 별도 설명)
MOD.grp1 <- group_by(MOD, condition)
summarise(MOD.grp1, n(), mean(IQ), sd(IQ))

MOD.grp2 <- group_by(MOD, D1, D2)
summarise(MOD.grp2, n(), mean(IQ), sd(IQ))

# Regression model with moderation
model1 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2 + MOD$WM + (MOD$D1*MOD$WM) + (MOD$D2*MOD$WM))
summary(model1)

# Regression model without moderation
model2 <- lm(MOD$IQ ~ MOD$D1 + MOD$D2 + MOD$WM)
summary(model2)

# ANOVA between two mdels
anova(model1, model2)
