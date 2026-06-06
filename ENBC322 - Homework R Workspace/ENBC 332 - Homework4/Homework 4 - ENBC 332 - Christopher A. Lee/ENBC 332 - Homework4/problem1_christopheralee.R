# Christopher A. Lee
# ENBC 332
# Homework 4 Problem 1
# 10/01/2025

# Link to dataset:
# https://www.kaggle.com/datasets/mosapabdelghany/medical-insurance-cost-dataset
install.packages("readxl")
library(readxl)
data <- read_excel('/Users/chrislee/Desktop/R Workspace/ENBC 332 - Homework2/ENBC 332 - HW02 Problem1 Excel File.xlsx', sheet = "Sheet1")

# Part A
covar_for <- function(temp_data) {
  total_dev <- 0
  mean_age <- mean(temp_data$age)
  mean_charges <- mean(temp_data$charges)
  col_length <- length(temp_data$age)
  for (i in 1:col_length) {
    total_dev <- total_dev + (temp_data$age[i] - mean_age) * (temp_data$charges[i] - mean_charges)
  }
  covariance <- total_dev / (col_length - 1)
  return(covariance)
}
covar_sum <- function(temp_data) {
  total_dev <- 0
  mean_age <- mean(temp_data$age)
  mean_charges <- mean(temp_data$charges)
  col_length <- length(temp_data$age)
  total_dev <- sum((temp_data$age - mean_age) * (temp_data$charges - mean_charges))
  covariance <- total_dev / (col_length - 1)
  return(covariance)
}
cat("Covariance for dataset using for loop:", covar_for(data))
cat("Covariance for dataset using sum function:", covar_sum(data))

# Part B
### Previous functions from homework 2
var_func_for <- function(data_col) {
  data_mean <- mean(data_col)
  total_subtracted_mean <- 0
  for (data_point in data_col) {
    total_subtracted_mean <- total_subtracted_mean + (data_point - data_mean)^2
  }
  data_variance <- total_subtracted_mean / (length(data_col) - 1)
  return(data_variance)
}
var_func_sum <- function(data_col) {
  data_mean <- mean(data_col)
  subtracted_mean_values <- (data_col - data_mean)
  squared_difference <- subtracted_mean_values^2
  total_subtracted_mean <- sum(squared_difference)
  data_variance <- total_subtracted_mean / (length(data_col) - 1)
  return(data_variance)
}
sd_func_for <- function(data_col) {
  data_mean <- mean(data_col)
  total_subtracted_mean <- 0
  for (data_point in data_col) {
    total_subtracted_mean <- total_subtracted_mean + (data_point - data_mean)^2
  }
  data_standard_deviation <- sqrt(total_subtracted_mean / (length(data_col) - 1))
  return(data_standard_deviation)
}
sd_func_sum <- function(data_col) {
  data_mean <- mean(data_col)
  subtracted_mean_values <- (data_col - data_mean)
  squared_difference <- subtracted_mean_values^2
  total_subtracted_mean <- sum(squared_difference)
  data_standard_deviation <- sqrt(total_subtracted_mean / (length(data_col) - 1))
  return(data_standard_deviation)
}
###

cor_coe_for <- function(temp_data) {
  cor_coe = covar_for(temp_data) / (sd_func_for(temp_data$age) * sd_func_for(temp_data$charges))
  return(cor_coe)
}
cor_coe_sum <- function(temp_data) {
  cor_coe = covar_sum(temp_data) / (sd_func_sum(temp_data$age) * sd_func_sum(temp_data$charges))
  return(cor_coe)
}
cat("Pearson's correlation coefficient for the dataset using for loops:", cor_coe_for(data))
cat("Pearson's correlation coefficient for the dataset using sum function:", cor_coe_sum(data))

# Part C
plot(data$age, data$charges,
     main="Scatter Plot of Coffee Charges in Relation to Age",
     xlab="Age",
     ylab="Charges",
     pch=19)
