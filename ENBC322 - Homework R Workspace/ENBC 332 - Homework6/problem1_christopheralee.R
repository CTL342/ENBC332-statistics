# Christopher A. Lee
# ENBC 332
# Homework 6 Problem 1
# 10/27/2025

# Variables
mean_pop <- 2300
n <- 30
intake_data <- c(2100, 2500, 2300, 1950, 2600, 2200, 2450, 2150, 2400, 2550, 
                 2350, 2250, 2050, 2650, 2300, 2400, 2100, 2500, 2350, 2200, 
                 2450, 2150, 2250, 2550, 2050, 2600, 2300, 2150, 2400, 2500)
mean_sam <- mean(intake_data)
sd_sam <- sd(intake_data)

# Computations
t_stat <- (mean_sam - mean_pop) / (sd_sam / sqrt(n))
p_value_t <- 2 * (1 - pt(abs(t_stat), df = n-1))

cat("T-test:", "\nT-Statistic:", t_stat, "\nP-Value:", p_value_t)
