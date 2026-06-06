# Christopher A. Lee
# ENBC 332
# Homework 5 Problem 1
# 10/20/2025

# Link to dataset:
# https://www.kaggle.com/datasets/mosapabdelghany/medical-insurance-cost-dataset
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

data <- read_excel('/Users/chrislee/Desktop/R Workspace/ENBC 332 - Homework2/ENBC 332 - HW02 Problem1 Excel File.xlsx', sheet = "Sheet1")
data_age = data$age

random_sample_values = sample(data_age, size=500, replace=FALSE)

# Part A
CI_func_zscore <- function(sample_data, conf_level) {
  sample_mean <- mean(sample_data)
  sample_size <- length(sample_data)
  sample_sd <- sd(sample_data)
  
  sample_standard_error <- sample_sd/sqrt(sample_size)
  alpha <- 1 - conf_level
  critical_z <- qnorm(1 - (alpha / 2))
  sample_moe <- critical_z * sample_standard_error
  
  lower_bound <- sample_mean - sample_moe
  upper_bound <- sample_mean + sample_moe
  return(cat("Confidence Interval for", conf_level * 100, "% \n[",lower_bound, upper_bound,"]\n"))
}

CI_zscore_ninety <- CI_func_zscore(random_sample_values, 0.9)
CI_zscore_ninety_five <- CI_func_zscore(random_sample_values, 0.95)
CI_zscore_ninety_nine <- CI_func_zscore(random_sample_values, 0.99)

random_sample_ten <- sample(data_age, size=10, replace=FALSE)
random_sample_fifty <- sample(data_age, size=50, replace=FALSE)
random_sample_onehundred <- sample(data_age, size=100, replace=FALSE)
random_sample_threehundred <- sample(data_age, size=300, replace=FALSE)
random_sample_fivehundred <- sample(data_age, size=500, replace=FALSE)
random_sample_onethousand <- sample(data_age, size=1000, replace=FALSE)

# Part B
CI_func_tscore_width <- function(sample_data, conf_level) {
  sample_mean <- mean(sample_data)
  sample_size <- length(sample_data)
  sample_sd <- sd(sample_data)
  
  if (sample_size < 2) {
    return(NA)
  }
  
  sample_standard_error <- sample_sd/sqrt(sample_size)
  alpha <- 1 - conf_level
  critical_t <- qt(1 - (alpha / 2), df = sample_size - 1)
  sample_moe <- critical_t * sample_standard_error
  
  return(2 * sample_moe)
}

ten_CI_tscore_ninety <- CI_func_tscore_width(random_sample_ten, 0.90)
ten_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_ten, 0.95)
ten_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_ten, 0.99)
fifty_CI_tscore_ninety <- CI_func_tscore_width(random_sample_fifty, 0.90)
fifty_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_fifty, 0.95)
fifty_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_fifty, 0.99)
onehundred_CI_tscore_ninety <- CI_func_tscore_width(random_sample_onehundred, 0.90)
onehundred_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_onehundred, 0.95)
onehundred_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_onehundred, 0.99)
threehundred_CI_tscore_ninety <- CI_func_tscore_width(random_sample_threehundred, 0.90)
threehundred_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_threehundred, 0.95)
threehundred_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_threehundred, 0.99)
fivehundred_CI_tscore_ninety <- CI_func_tscore_width(random_sample_fivehundred, 0.90)
fivehundred_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_fivehundred, 0.95)
fivehundred_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_fivehundred, 0.99)
onethousand_CI_tscore_ninety <- CI_func_tscore_width(random_sample_onethousand, 0.90)
onethousand_CI_tscore_ninety_five <- CI_func_tscore_width(random_sample_onethousand, 0.95)
onethousand_CI_tscore_ninety_nine <- CI_func_tscore_width(random_sample_onethousand, 0.99)

plot_data <- data.frame(
  sample_size = c(10, 50, 100, 300, 500, 1000,
                  10, 50, 100, 300, 500, 1000,
                  10, 50, 100, 300, 500, 1000),
  
  conf_level = as.factor(c(rep(90, 6),
                           rep(95, 6),
                           rep(99, 6))),
  
  ci_width = c(ten_CI_tscore_ninety, fifty_CI_tscore_ninety, onehundred_CI_tscore_ninety,
               threehundred_CI_tscore_ninety, fivehundred_CI_tscore_ninety, onethousand_CI_tscore_ninety,
               
               ten_CI_tscore_ninety_five, fifty_CI_tscore_ninety_five, onehundred_CI_tscore_ninety_five,
               threehundred_CI_tscore_ninety_five, fivehundred_CI_tscore_ninety_five, onethousand_CI_tscore_ninety_five,
               
               ten_CI_tscore_ninety_nine, fifty_CI_tscore_ninety_nine, onehundred_CI_tscore_ninety_nine,
               threehundred_CI_tscore_ninety_nine, fivehundred_CI_tscore_ninety_nine, onethousand_CI_tscore_ninety_nine)
)

ggplot(plot_data, aes(x = sample_size, y = ci_width, color = conf_level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3)
