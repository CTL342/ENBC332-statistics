# Christopher A. Lee
# ENBC 332
# Homework 6 Problem 2
# 10/27/2025

# Variables
mean_pop <- 120
n <- 25
ldl_data <- c(110, 112, 115, 113, 111, 114, 109, 112, 111, 115, 113, 110, 
              112, 109, 111, 113, 114, 110, 112, 111, 109, 113, 112, 110, 111)
mean_sam <- mean(ldl_data)
sd_sam <- sd(ldl_data)

# Computations
t_stat <- (mean_sam - mean_pop) / (sd_sam / sqrt(n))
p_value_t <- (1 - pt(abs(t_stat), df = n-1))

cat("T-test:", "\nT-Statistic:", t_stat, "\nP-Value:", p_value_t)
