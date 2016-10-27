# Statistics One, 2013, Lab 8

# Lab goals
#   Conduct group comparisons
#     Dependent t-tests
#     Independent t-tests
#     Analysis of Variance (ANOVA)

# Example
#  Working memory training experiment (N = 120)
#  The dependent variable (DV) is number of items answered correctly on an intelligence test
#  There are three independent variables:
#    Time (2 levels): pre and post training
#    Training (2 levels): training (1) and control (0) (n.training = 80, n.control = 40)
#    Training sessions (4 levels): 8, 12, 17, 19 (for each, n = 20)

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("Users/aconway/Dropbox/STATS1-V2.0/Labs")

# If necessary, install packages
# install.packages("psych")
# install.packages("car")

# Load packages
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(shiny)
library(ggvis)
library(psych)
library(car)
library(lsr)
library(data.table)

# Read data into a dataframe called wm

setwd("C:/Users/Administrator/Documents/R")
wm <- read.table("stats1-datafiles-Stats1.13.Lab.08.txt", header = T)
tbl_df(wm)

wm.sm <- group_by(wm, train)
summarise(wm.sm, n(), mean(gain))

# If you want to view the data
glimpse(wm)
head(wm, n=8)

# ------------------------------------------------------------------------------------------
# Dependent t-tests
# Compare pre and post scores in the training groups
wm1 <- filter(wm, train=="1") # training group 80 추출 (train=="1")
glimpse(wm1)
head(wm1, 8)

# Cohen's d for dependent t-tests
# d = Mean of difference scores / Standard deviation of difference scores
cohensD(wm$post, wm$pre, method="paired")

# ------------------------------------------------------------------------------------------
# Independent t-test#1
glimpse(wm)
t.test(wm$gain ~ wm$train, var.equal = T)

# Cohen's d for independent t-tests
# d = (M1 - M2) / Pooled Standard Deviation
cohensD(wm$gain ~ wm$train, method="pooled")

# ------------------------------------------------------------------------------------------
# To compare the gain scores across all groups, use ANOVA
aov.model <- aov(wm1$gain ~ wm1$cond)
summary(aov.model)
etaSquared(aov.model, anova=T)

# Conduct post-hoc tests to evaluate all pairwise comparisons
TukeyHSD(aov.model)

# Check the homogeneity of variance assumption
leveneTest(wm1$gain, wm1$cond, center="mean")
leveneTest(wm1$gain, wm1$cond)
