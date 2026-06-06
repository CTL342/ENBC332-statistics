# Christopher A. Lee
# ENBC 332
# Homework 8 Problem 2
# 11/11/2025

# Data
set.seed(123)
BMI <- rnorm(300, mean = 25, sd = 5)
SBP <- 90 + 2.5 * BMI + rnorm(300, mean = 0, sd = 8)

# Part A
df <- data.frame(BMI, SBP)
lin_reg <- lm(SBP ~ BMI, data=df)
summary(lin_reg)

# Part B
plot(lin_reg)
