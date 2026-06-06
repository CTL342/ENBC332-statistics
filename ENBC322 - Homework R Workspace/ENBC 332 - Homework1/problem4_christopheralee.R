# Link to dataset: 
# https://www.kaggle.com/datasets/aishu200023/stackindex
install.packages("readxl")
library(readxl)

my_data <- read_excel("/Users/chrislee/Desktop/R Workspace/Homework1 - ENBC 332/ENBC 332 - HW01 Problem4 Excel File.xlsx", sheet = "Sheet1")
plot(my_data)

mean(my_data$month)
mean(my_data$matlab)
median(my_data$month)
median(my_data$matlab)
sum(my_data$month)
sum(my_data$matlab)
