#The "----" make the ide list the information 
#pre processing ----
##Loading packages and libraries ----
install.packages("randomForest")
install.packages("xgboost")
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(xgboost)

##Loading the data----
setwd("/Users/keksmacbookair/Desktop/course works/R Programming/Practice R with Charles/R-practice")
data <-read.csv("pricingofDiamonds.csv")
#review few rows of the data ----
view (data)
head(data,n=10)#to see the first 10 rows
#view the structure of the data set----
str(data)#has ints ,chars and nums
#summary statistics
summary(data)#at this point we can use statistics to already tell if the data has outliers

##Handling missing values----
#check for missing data
sum(is.na(data))
#has no missing data
#approach one:----
missing_values <-sapply(data, function(x) sum(is.na(x)))#using a missing value function
missing_values
#no missing values in all columns
#approach two----
missing_values_2 <- data %>%summarise(numeric_missing = sum(is.na(.)),(categorical_missing = sum(is.na(as.character(.)))))#noint
missing_values_2
#approach 3 using dply ----
missing_values_3 <-data %>% summarise_all(funs(sum(is.na(.))))
missing_values_3
#a dot is neccessary for the code to run....


##Remove missing values----
##Drop missing values
#method 1 ----
data2 <-na.omit(data)

#method2(imputation with either mean,median or mode) ----
#This visualises the data to easily see 
par(mfrow = c(2,1))## Helps get them all onto one page
hist(data$price, col = "blue", main = "Histogram for price")

hist(data$depth, col="blue", main = "Histogram for depth")

hist(data$x, col="blue", main = "Histogram for x")
#The graphs help tell us what we are to use on each specific variable , left skew = ,right skew = ,balanced = mean

##imputing----
###1,Continous data----
#With. mean (creating a function----
impute_mean <- function(x) replace(x,is.na(x), mean(x,na.rm = TRUE))
#with median ----
impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
###2,CATEGORICAL VARIBLES
impute_mode <- function(x) {
  model_value <-as.numeric(names(table(sort(x),decreasing = TRUE)[1]))##we convert the numeric variable into numeric so that it can be counted
  ##The true value is 1 cause in R it starts counting from the one value
  replace(x,is.na(x),model_value)
}
 
#call the functions
impute_mean(data$depth)
impute_median(data$price)

#Approach three is using the mice function that technixally reates another data set----
imputed_data <- mice(data.m = 5, method ="pmm",maxit = 50, seed = 500)




######Dealing with outliers ----
###Visualisation
numeric_data <- data %>% select_if(is.numeric) #A quick way to grab all the numeric variables in one sweep for cleaning
par(mfrow = c(ceiling(sqrt(ncol(numeric_data))), ceiling(sqrt(ncol(numeric_data)))))
#a ceiling number is the greatest integer of a given float while the floor number is the least integer of a float
for (i in  1:ncol(numeric_data)){
  boxplot(numeric_data[,i], main = colnames(numeric_data)[i])
}
data  <- data[,!names(data) %in% ("ID")]
head(data)
###boxplot 2----
boxplot_gg <function(data,columns){
  for (column in columns){
    p <- ggplot(data = data, mapping =aes(x = "" , y = .data[[column]])) +
      geom_boxplot() +
      laabs(title = paste("Boxplot for  ", column))
    print(p)
  }
  
}
boxplot_gg(data, c("carat",))

##not completed 
#####Removing the outliers----
remove_outliers <- function(entered_data,columns){
  for (column in columns){
  quantiles <- quantile(entered_data[[column]], probs = c(0.25, 0.75)) 
  iqr_feature <- quantiles[2]- quantiles[1]
  upper_boundary <- quantiles[2] + 1.5*iqr_feature
  lower_boundary <- quantiles[1] - 1.5*iqr_feature
  entered_data <- entered_data[which((entered_data[[column]] >= lower_boundary) & entered_data[[column]] <= upper_boundary), ]
  }
  return (entered_data)
}
cleaned_data <- remove_outliers(data, c('carat','depth','price','x','y'))

##Plotting the cleaned data set----
numeric_data_2 <- cleaned_data %>% select_if(is.numeric) #selecting the numeric variables form our cleaned data set
par(mfrow = c(ceiling(sqrt(ncol(numeric_data_2))), ceiling(sqrt(ncol(numeric_data_2)))))
#a ceiling number is the greatest integer of a given float while the floor number is the least integer of a float
for (i in  1:ncol(numeric_data_2)){
  boxplot(numeric_data_2[,i], main = colnames(numeric_data_2)[i])
}













########Relationships. #correlation and convention----
##Between two continous
###using a coorelation matrix or scatter plots
## Between continous and categorical - use boxplots and anova test
##cat and cat (use bar plots , contingency tests ,phi square test )
##means(cont) use the shapiro will test 


###Between a continous and a categorical----
#our target variable is PRICE of diamonds- it is a continous variable 
categorical_variable <- cleaned_data %>% select_if(is.character)
view(categorical_variable)
str(categorical_variable)
#now we perform the anova test by creating a function
perform_anova <- function(data, cont_var , cat_vars){
  for (cat_var in cat_vars){
  anova_result  = aov(as.formula(paste(cont_var , '~' , cat_var )) , data = data)
  summary_anova <- summary(anova_result)
  p_value <- summary_anova [[1]]["Pr(>F)"][1, ]
  
  if (p_value <= 0.05){
    cat("Significant relationship found between ",cont_var , "and " , cat_var,"with a p value ",p_value)
    }
  
  else {
    cat("No significant relationship found between",cont_var,"and",cat_var, "with a p value", p_value)
  }

}

}

perform_anova (cleaned_data , 'price', c('cut','colour','clarity','P','PC'))
  
###Between two continous variables ----
##using the correlation matrix
correlation_matrix <- cor(numeric_data) 
correlation_matrix 
##using pairs
pairs(cleaned_data[,c('x','y','carat','price','depth')])
##using correlation plot(i like this one for real)
corrplot(correlation_matrix, method = "circle")
#we drop depth cause it had only 3percent (0.03) correlation to pricesince price is our target variable

###Between two categoricals----
view(cleaned_data)
perform_chisq <-function(data,cat_var_1,cat_data){
  for (cat_var_2 in cat_data) {
  chisq_result = chisq.test(table(cleaned_data[[cat_var_1]], cleaned_data[[cat_var_2]]))
  p_value <- chisq_result$p.value 
    
  if (p_value <= 0.05){
    cat("Significant relationship found between",cat_var_1 ,"and", cat_var_2)
    }
  else {
    cat("Significant relationship not found between", cat_var_1 ,"and",cat_var_2)
    }
  }
}
  
perform_chisq(cleaned_data,'colour',c('cut','clarity','P','PC'))
#for the machine learning model we remove the data with little to no correlation in this case depth


#####Creating the machine learning data ----
ml_data <-cleaned_data[,!names(cleaned_data) %in% c("depth","P","PC")]
head(ml_data)























  
  
  
  
  
  
  













