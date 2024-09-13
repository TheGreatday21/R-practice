library(pacman)
library(dlookr)
library(Hmisc)
library(corrplot)




setwd("/Users/keksmacbookair/Desktop/course work/Intro to Data science/data sets")

data1 <- read_excel("Vegetable_Sales-1.xlsx")

head(data1)
describe(data1)
str(data1)
summary(data1)

#turn character variables to factor
data1$`Item Name` <- as.factor(data1$`Item Name`)
data1$`Sale or Return` <- as.factor(data1$`Sale or Return`)
data1$`Discount (Yes/No)` <- as.factor(data1$`Discount (Yes/No)`)
str(data1)

##whats the highest selling vegetable
max(data1$`Quantity Sold (kg)`)
#max quantity sold = 1.321
high_veggie <- filter(data1, data1$`Quantity Sold (kg)`== 1.321)
high_veggie
##the highest selling veggie was Haixian Mushroom
discount <- filter(data1,data1$`Discount (Yes/No)` == "yes")
count(discount)
#6 vegetables were sold on discount

#quantities sold in kg
summary(data1$`Quantity Sold (kg)`)
shapiro.test(data1$`Quantity Sold (kg)`)
##p value is below 0.05

max(data1$`Loss Rate (%)`)
remove <- filter(data1,data1$`Loss Rate (%)`==29.25)
remove
##they should remove high melon























