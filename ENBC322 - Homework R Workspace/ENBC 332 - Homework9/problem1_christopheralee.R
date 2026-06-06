# Christopher A. Lee
# ENBC 332
# Homework 9 Problem 1
# 11/22/2025

# Given Dataset
Recovery_Time = c(10, 8, 15, 12, 7, 14, 9, 11, 6, 13)
Treatment = c("Placebo", "Drug", "Placebo", "Drug", "Drug", "Placebo", "Drug", "Placebo", "Drug", "Placebo")
Gender = c("Male", "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female", "Male")

# Part A
df_drug <- data.frame(Recovery_Time, Treatment, Gender)
df_drug$Treatment <- factor(df_drug$Treatment, levels=c("Placebo", "Drug"))
df_drug$Gender <- factor(df_drug$Gender, levels=c("Female", "Male"))

model_drug <- lm(Recovery_Time ~ Treatment + Gender, data=df_drug)
summary(model_drug)