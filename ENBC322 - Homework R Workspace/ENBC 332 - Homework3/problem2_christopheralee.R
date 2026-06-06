# Christopher A. Lee
# ENBC 332
# Homework 3 Problem 2
# 09/22/2025

# Librares and Initializations
install.packages("ggplot2")
library(ggplot2)

# Variables
mean_val <- 50
sd_val <- 10

# Plot
x <- seq(mean_val - 4*sd_val, mean_val + 4*sd_val, length.out = 1000)
y <- dnorm(x, mean = mean_val, sd = sd_val)
df <- data.frame(x, y)

# Part A
# Calculations for probability between 40 and 60
prob_a <- pnorm(60, mean = mean_val, sd = sd_val) - pnorm(40, mean = mean_val, sd = sd_val)
cat("Probability between 40 and 60:", prob_a)

# Generate plot that highlights the area between 40 and 60
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_ribbon(data = subset(df, x > 40 & x < 60), aes(ymin = 0, ymax = y), fill = "yellow", alpha = 0.3)

# Part B
# Calculations for probability between 5 and 75
prob_b <- pnorm(75, mean = mean_val, sd = sd_val) - pnorm(5, mean = mean_val, sd = sd_val)
cat("Probability between 5 and 75:", prob_b)

# Generate plot that highlights the area between 5 and 75
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_ribbon(data = subset(df, x > 5 & x < 75), aes(ymin = 0, ymax = y), fill = "yellow", alpha = 0.3)

# Part C
# Calculations for probability greater than 60
prob_c <- 1 - pnorm(60, mean = mean_val, sd = sd_val)
cat("Probability greater than 60:", prob_c)

# Generate plot that highlights the area greater than 60
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_ribbon(data = subset(df, x > 60), aes(ymin = 0, ymax = y), fill = "yellow", alpha = 0.3)

# Part D
# Calculations for probability less than 90
prob_d <- pnorm(90, mean = mean_val, sd = sd_val)
cat("Probability less than 90:", prob_d)

# Generate plot that highlights the area less than 90
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_ribbon(data = subset(df, x < 90), aes(ymin = 0, ymax = y), fill = "yellow", alpha = 0.3)
