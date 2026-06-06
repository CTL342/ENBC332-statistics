# Christopher A. Lee
# ENBC 332
# Homework 8 Problem 1
# 11/10/2025

# Data
ad_num <- c(1, 2, 3, 4, 5, 6)
mon_sal <- c(3, 5, 7, 10, 12, 14)

# Part A
df <- data.frame(ad_num, mon_sal)
plot(df$ad_num, df$mon_sal, main="Scatter Plot", xlab="Number of Advertisements", ylab="Monthly Sales in $1000")

# Part B
lin_reg <- function(x_data, y_data) {
  x_mean <- mean(x_data)
  y_mean <- mean(y_data)
  
  x_diff <- x_data - x_mean
  y_diff <- y_data - y_mean
  
  num <- sum(x_diff * y_diff)
  den <- sum(x_diff^2)
  
  m <- num / den
  
  return(c(m, y_mean - m*x_mean))
}
lin_reg_custom <- lin_reg(df$ad_num, df$mon_sal)
lin_reg_func <- lm(mon_sal ~ ad_num, data=df)
slope <- lin_reg_custom[1]
y_intercept <- lin_reg_custom[2]

cat("Custom Function Values\nSlope:", slope,"\nY-Intercept:", y_intercept, "\n")
summary(lin_reg_func)

# Part C
cat("Sale Prediction for 4 Advertisements in a Month:", slope * 4 + y_intercept,
    "\nSale Prediction for 6 Advertisements in a Month:", slope * 6 + y_intercept)

# Part D
predicted_values <- slope * df$ad_num + y_intercept
pre_val_mean <- mean(predicted_values)
SST <- sum((df$mon_sal - mean(df$mon_sal))^2)
SSR <- sum((predicted_values - mean(df$mon_sal))^2)
SSE <- sum((df$mon_sal - predicted_values)^2)

R2 <- SSR / SST
cat("R Squared Value:", R2)