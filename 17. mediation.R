# Statistics One, 2013, Lab 7

# Lab goals
#   Conduct mediation analysis

#   Mediation analysis
#     Example
#       An experimental research investigation of the effects of stereotype threat on intelligence testing 
#         Dependent variable (Y) is score on an intelligence test (IQ)
#         Independent variable (X) is the treatment condition (2 levels: control, threat)
#         Mediator variable (M) is score on a working memory task
#         Sample size of N = 100 (n = 50)

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
library(dplyr)
library(ggplot2)
library(multilevel)


# Read data into a dataframe called MED
MED <- read.table("stats1-datafiles-Stats1.13.Lab.07b.txt", header = T)
head(MED, 8)
MED <- select(MED, subject, condition, WM, IQ)
# If you want to view the data
View(MED)

# Summary statistics
MED.grp <- group_by(MED, condition)
summarise(MED.grp, count = n(), mean_WM = mean(WM), mean_IQ = mean(IQ))

# The function sobel in the multilevel package executes the entire mediation analysis in one step but first we will do it with 3 lm models
model.YX <- lm(MED$IQ ~ MED$condition)
model.YXM <- lm(MED$IQ ~ MED$condition + MED$WM)
model.MX <- lm(MED$WM ~ MED$condition)

summary(model.YX)
summary(model.YXM)
summary(model.MX)

# Compare the results to the output of the sobel function
model.ALL <- sobel(MED$condition, MED$WM, MED$IQ) 
model.ALL
