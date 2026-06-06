# Christopher A. Lee
# ENBC 332
# Homework 3 Problem 1
# 09/22/2025

# Part A
install.packages("ggplot2")
library(ggplot2)

data <- rnorm(5000, mean = 75, sd = 7)
df <- data.frame(value = data)
first_plot <- ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "white", color = "black") +
  geom_density(color = "red", size = 1.2)

# Part B
mean_data <- mean(data)
sd_data <- sd(data)
z_score1 <- (68 - mean_data) / sd_data
z_score2 <- (75 - mean_data) / sd_data
z_score3 <- (96 - mean_data) / sd_data
cat("Z Score at 68:", z_score1)
cat("Z Score at 75:", z_score2)
cat("Z Score at 96:", z_score3)

data_points <- c(68, 75, 96)

vertical_lines <- data.frame(
  x_intercept = data_points,
  label = paste("Value:", data_points)
)

final_plot <- first_plot +
  geom_vline(
    data = vertical_lines,
    aes(xintercept = x_intercept, color = label), # Map color to the label
    size = 1.2,
    linetype = "dashed",
  )
print(final_plot)

# Part C
# Please refer to the Problem 1 Part C of the discussion file