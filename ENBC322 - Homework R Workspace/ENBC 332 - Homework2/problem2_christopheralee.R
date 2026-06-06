# Christopher A. Lee
# ENBC 332
# Homework 2 Problem 2
# 09/15/2025

n <- sample(5:20, 1)
r <- sample(1:n, 1)
print(n)
print(r)

total_poss <- 1
num_to_multiply = n
for(i in 1:r) {
  total_poss <- total_poss * num_to_multiply
  num_to_multiply <- num_to_multiply - 1
}
print("Total Permutations:")
print(total_poss)
