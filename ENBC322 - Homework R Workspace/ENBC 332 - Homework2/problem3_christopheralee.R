# Christopher A. Lee
# ENBC 332
# Homework 2 Problem 3
# 09/15/2025

die1 <- sample(1:6, 1)
die2 <- sample(1:6, 1)
print(paste("Die 1:", die1))
print(paste("Die 2:", die2))

prob_two_dice_func <- function(num1, num2) {
  total <- num1 + num2
  counter <- 0
  for(i in 1:6) {
    for(j in 1:6) {
      if(i + j == total) {
        counter <- counter + 1
      }
    }
  }
  prob = counter / 36
  return(prob)
}
prob_two_dice_func(die1, die2)
