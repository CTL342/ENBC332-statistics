# Christopher A. Lee
# ENBC 332
# Homework 9 Problem 2
# 11/22/2025

# Given Dataset
set.seed(123) # For reproducibility
Price = round(rnorm(50, mean = 300, sd = 50), 2)
Size = round(runif(50, 800, 4000), 2)
Bedrooms = sample(2:6, 50, replace = TRUE)
Age = sample(1:50, 50, replace = TRUE)
Distance_to_City_Center = round(runif(50, 1, 30), 2)

# Part A
df_house <- data.frame(Price, Size, Bedrooms, Age, Distance_to_City_Center)

model_house <- lm(Price ~ Size + Bedrooms + Age + Distance_to_City_Center, data=df_house)
summary(model_house)