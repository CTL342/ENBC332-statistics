# Christopher A. Lee
# ENBC 332
# Homework 2 Problem 1
# 09/15/2025

# Link to dataset:
# https://www.kaggle.com/datasets/mosapabdelghany/medical-insurance-cost-dataset
install.packages("readxl")
library(readxl)
data <- read_excel('/Users/chrislee/Desktop/R Workspace/Homework2 - ENBC 332/ENBC 332 - HW01 Problem1 Excel File.xlsx', sheet = "Sheet1")

# Part A
range(data$age)
range(data$charges)

# Part B
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
var(data$age)
var(data$charges)
var_func_for(data$age)
var_func_for(data$charges)
var_func_sum(data$age)
var_func_sum(data$charges)

# Part C
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
sd(data$age)
sd(data$charges)
sd_func_for(data$age)
sd_func_for(data$charges)
sd_func_sum(data$age)
sd_func_sum(data$charges)

# Part D
quantile_func <- function(data_col) {
  sorted_col <- sort(data_col)
  value1 = min(data_col)
  value3 = median(data_col)
  value5 = max(data_col)
  
  midpoint <- ceiling(length(sorted_col) / 2)
  
  value2 = median(sorted_col[1:midpoint])
  value4 = median(sorted_col[(length(sorted_col) - midpoint + 1): length(sorted_col)])
  
  return(list(value1, value2, value3, value4, value5))
}
quantile(data$age)
quantile(data$charges)
quantile_func(data$age)
quantile_func(data$charges)

# Part E
iqr_func <- function(data_col) {
  sorted_col <- sort(data_col)
  midpoint <- ceiling(length(sorted_col) / 2)
  
  value1 = median(sorted_col[1:midpoint])
  value2 = median(sorted_col[(length(sorted_col) - midpoint + 1): length(sorted_col)])
  
  data_iqr = value2 - value1
  return(data_iqr)
}
IQR(data$age)
IQR(data$charges)
iqr_func(data$age)
iqr_func(data$charges)
