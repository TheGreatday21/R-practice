#install and call necessary packages
install.packages("plotly")
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(randomForest)
library(Hmisc)
library(dlookr)

#set a working directory and call up the data set
setwd("/Users/keksmacbookair/Desktop/course work/Intro to Data science/data sets")
data1 <-read_excel("Wholesale customers data.xlsx")
#explore the data 
head(data1)
view(data1)
summary(data1)
str(data1)
plot(data1)

#q1.Check to see if all the continuous variables are normally distributed. (3 MARKS)----
###using dplyr to identify the continous varibles----
cont_vars <- data1%>%select (Fresh,Grocery,Milk,Frozen,Detergents_Paper,Delicassen)
cont_vars

#Or we can use the select function
cont_vars <- select(data1,Fresh:Delicassen)
cont_vars

##checking using the shapiro wilk test##this test is only used on data less than 5000 observations
shapiro.test(cont_vars$Fresh)
shapiro.test(cont_vars$Grocery)
shapiro.test(cont_vars$Milk)
shapiro.test(cont_vars$Frozen)
shapiro.test(cont_vars$Detergents_Paper)
shapiro.test(cont_vars$Delicassen)
##all the continous variables p values were below o.o5 hence they are not normally distributed

##  OR
hist(cont_vars)
##the histogram plots show how they are deviating from their means  in this case all the continuous variables are left skewed
## OR using the describe function
descriptive_cont_vars = describe(cont_vars)
descriptive_cont_vars
#Shows all the variables skewness is positive but not 1 therefore they are all positive/rigth skewed


###checking distribution of the categorical variables----
cat_vars <- select(data1,Channel,Region)
cat_vars
ggplot(data = data1, mapping = aes(x=Channel)) + geom_boxplot() + labs(x ="Channel",title = "boxplot for categorical")
#or
boxplot(data1$Channel)


##q2.Transform the data set to exclude any missing information (1 MARK)----
#check for missing data
sum(is.na(data1))
colSums(is.na(data1))#checking specific column to see which is missing data
##the data set has no missing data



##q3.Show the outliers in only the continuous variable in the dataset (3 MARKS)----
boxplot(data1)$out
#this shows the outliers in each continouos variable visual
  
###create a detect outliers function----
detect_outliers = function(x) {
  Q1 <- quantile(x, prob = .25)
  Q3 <- quantile(x, prob = .75)
  IQR = Q3 - Q1
  lower_boundary <- Q1 - 1.5*IQR
  upper_boundary <- Q3 + 1.5*IQR
  x[ x<lower_boundary | x>upper_boundary ]
}
##use the function on the continuous variables data set
out_cont_vars = lapply(cont_vars,detect_outliers)
out_cont_vars  #3this will show the outliers in each variable in the continuos variables data set
count(out_cont_vars)
#Fresh had 21 outliers
#Milk had 28 outliers
#Grocery had 24 outliers
#Frozen had 44 outliers
  
  
  
##q4.Transform the data set and handle the outliers in the continuous variables if any. (3 MARKS)----


###create a remove outliers function----


























