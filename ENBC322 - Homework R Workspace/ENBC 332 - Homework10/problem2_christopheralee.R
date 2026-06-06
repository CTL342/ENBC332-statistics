# Christopher A. Lee
# ENBC 332
# Homework 10 Problem 2
# 12/03/2025

# Given data
install.packages("readxl")
library(readxl)
df <- read_excel('/Users/christopherlee/Desktop/R Workspace/ENBC 332 - Homework10/medical_dataset.xlsx', sheet = "Sheet1")

anova_model <- aov(Reduction_mmHg ~ Group, data = df)
summary(anova_model)
