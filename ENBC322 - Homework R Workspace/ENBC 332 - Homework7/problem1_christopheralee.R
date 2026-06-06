# Christopher A. Lee
# ENBC 332
# Homework 7 Problem 1
# 11/04/2025

# Variables
before <- c(78, 85, 92, 68, 74, 88, 81, 76, 95, 70,
            79, 82, 91, 72, 80, 83, 77, 89, 84, 73)
after <- c(75, 81, 89, 66, 71, 84, 78, 73, 90, 68,
           76, 79, 87, 70, 77, 80, 74, 86, 80, 71)

t_test_result <- t.test(before, after, paired = TRUE, alternative = "greater")
t_score <- t_test_result$statistic
p_value <- t_test_result$p.value

cat("Results:\nt-score:", t_score, "\np-value:", p_value)