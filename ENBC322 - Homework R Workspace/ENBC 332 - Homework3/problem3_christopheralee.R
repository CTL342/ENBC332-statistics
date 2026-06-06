# Christopher A. Lee
# ENBC 332
# Homework 3 Problem 3
# 09/22/2025

# Librares and Initializations
install.packages("ggplot2")
library(ggplot2)

# Variables
prob_of_success <- 0.7
trials <- 20

# Part A
# Probability calculations for exactly 15 patients respond positively
prob_a <- dbinom(15, size = trials, prob = prob_of_success)
cat("Probability exactly 15 patients respond positively:", prob_a)

# Part B
# Probability calculations for more than 12 patients respond positively
prob_b <- dbinom(13, size = trials, prob = prob_of_success) +
  dbinom(14, size = trials, prob = prob_of_success) +
  dbinom(15, size = trials, prob = prob_of_success) +
  dbinom(16, size = trials, prob = prob_of_success) +
  dbinom(17, size = trials, prob = prob_of_success) +
  dbinom(18, size = trials, prob = prob_of_success) +
  dbinom(19, size = trials, prob = prob_of_success) +
  dbinom(20, size = trials, prob = prob_of_success) +
cat("Probability more than 12 patients respond positively:", prob_b)

# Part C
# Probability calculations between 10 to 18 (inclusive) patients respond positively
prob_c <- dbinom(10, size = trials, prob = prob_of_success) +
  dbinom(11, size = trials, prob = prob_of_success) +
  dbinom(12, size = trials, prob = prob_of_success) +
  dbinom(13, size = trials, prob = prob_of_success) +
  dbinom(14, size = trials, prob = prob_of_success) +
  dbinom(15, size = trials, prob = prob_of_success) +
  dbinom(16, size = trials, prob = prob_of_success) +
  dbinom(17, size = trials, prob = prob_of_success) +
  dbinom(18, size = trials, prob = prob_of_success)
cat("Probability between 10 to 18 (inclusive) patients respond positively:", prob_c)

# Part D
# Plot for 30 percent probability of success
data1 <- rbinom(1000, size = trials, prob = 0.3)
df1 <- data.frame(successes = data1)
ggplot(df1, aes(x = successes)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black", alpha = 0.7)

# Plot for 50 percent probability of success
data2 <- rbinom(1000, size = trials, prob = 0.5)
df2 <- data.frame(successes = data2)
ggplot(df2, aes(x = successes)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black", alpha = 0.7)

# Plot for 70 percent probability of success
data3 <- rbinom(1000, size = trials, prob = 0.7)
df3 <- data.frame(successes = data3)
ggplot(df3, aes(x = successes)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black", alpha = 0.7)
