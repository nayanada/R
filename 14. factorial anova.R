# Statistics One, 2013, Lab 9

# Lab Goals
#    Conduct a between groups factorial ANOVA
#    Example
#    A randomized controlled experiment designed to investigate teh effects of talking on a cell phone while driving
#      DV = Number of driving errors
#      Two IVs
#         (A) Conversation difficulty (3 levels): Control, Easy, Difficult
#         (B) Driving difficulty (2 levels): Easy, Difficult

# If necessary, install packages
# install.packages("psych")
# install.packages("car")
# install.packages("lsr")

library(psych)
library(car)
library(lsr)
library(dplyr)

getwd()
# Read data into a dataframe called AB
AB <- read.table("stats1-datafiles-Stats1.13.Lab.09.txt", header = T)

# Let's look at the data 
glimpse(AB)
head(AB, 10)
AB.grp <- group_by(AB, conversation, driving)
summarise(AB.grp, n(), mean(errors))

# Test the homogeneity of variance assumption
leveneTest(AB$errors ~ AB$conversation * AB$driving)

# Conduct the factorial ANOVA
AB.model <- aov(AB$errors ~ AB$driving * AB$conversation)
summary(AB.model)

# Effect size
etaSquared(AB.model, anova = T)

# Pariwise comparison
TukeyHSD(AB.model)

##### interaction #####

# Conduct simple effects analysis (of Conversation at each level of Driving)
AB1 <- filter(AB, AB$conversation == "None")
AB2 <- filter(AB, AB$conversation == "Low demand")
AB3 <- filter(AB, AB$conversation == "High demand")

AB.model1 <- aov(AB1$errors ~ AB1$driving)
AB.model2 <- aov(AB2$errors ~ AB2$driving)
AB.model3 <- aov(AB3$errors ~ AB3$driving)

summary(AB.model1)
summary(AB.model2)
summary(AB.model3)

# Why is there an interaction? Let's look at effect sizes:
round(etaSquared(AB.model1), 3)
round(etaSquared(AB.model2), 3)
round(etaSquared(AB.model3), 3)
