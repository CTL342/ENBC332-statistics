# Christopher A. Lee
# ENBC 332
# Homework 7 Problem 2
# 11/04/2025

# Variables
treatment_group <- c(-12, -8, -10, -5, -7, -13, -9, -11, -6, -10, -15, -8, -11, 
                     -14, -9, -13, -8, -12, -10, -7, -11, -9, -14, -10, -12)
control_group <- c(-3, -4, -2, -5, -6, -3, -4, -5, -2, -1, -3, -4, -6, 
                   -3, -2, -4, -5, -2, -3, -1, -4, -3, -6, -2, -4)

# Computations
t_test_result <- t.test(treatment_group, control_group, alternative = "less")
t_score <- t_test_result$statistic
p_value <- t_test_result$p.value

cat("Results:\nt-score:", t_score, "\np-value:", p_value)
