
#install and call neccessary libraries
install.packages("pacman")
library(pacman)
#set a working directory
setwd("/Users/keksmacbookair/Desktop/course work/Intro to Data science/data sets")
data3 <-read_excel("Loan_Approval_Data.xlsx")
#q1.Explore the dataset and identify the datatypes of each variable (1 MARK)----
head(data3)
str(data3)
summary(data3)
#turn the char variables to as factor
data3$Loan_ID <- as.factor(data3$Loan_ID)
data3$Gender <- as.factor(data3$Gender)
data3$Married <- as.factor(data3$Married)
data3$Dependents <- as.factor(data3$Dependents)
data3$Education <- as.factor(data3$Education)
data3$Self_Employed <- as.factor(data3$Self_Employed)
data3$Loan_Status <- as.factor(data3$Loan_Status)

str(data3)

#q2.Transform the dataset to remove missing data and outliers if any  (3 MARKS)----
#check for missing data 
colSums(is.na(data3))
sum(is.na(data3))
#there are 150 missing values
#this is more than 30 % so we dont just delete it
##for the continous variables we impute with the median
data3_median <- data3 %>% mutate_if(is.numeric,function(x)ifelse(is.na(x),median(x,na.rm = T),x))
colSums(is.na(data3_median))
#for the categorical data we impute with the mode
data3_mode <- data3_median %>% mutate_if(is.factor(data3_median),function(x) ifelse(is.na(x), mode(x,na.rm = T),x))


##we can just remove the missing data since its less than 30%

data4 <- na.omit(data3)
str(data4)
sum(is.na(data4))
#no more missing data

##handling outliers
##create a detect outliers function

detect_outliers <- function(x){
  Q1 <- quantile(x, prob = .25)
  Q3 <- quantile (x, prob=.75)
  IQR = Q3 - Q1
  
  x > Q3 + (IQR * 1.5) | x < Q1 - (IQR * 1.5)
  
}

##create a remove outliers function

remove_outliers <- function(dataframe,columns = names(dataframe) ){
  for (col in columns){
    dataframe <- dataframe[!detect_outliers(dataframe[[col]]),]
  }

print("Remove outliers")
print(dataframe)
}

data5 <- remove_outliers(data4,c('ApplicantIncome', 'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term', 'Credit_History'))
#remove from the continous variables and. numeric

#q5.Save the transformed dataset as a new dataset (either csv or excel) (1 MARK)----
write_csv(data5,"Loan_Approval_Data.csv")

























#q6.Show relationships between continuous variables (5 MARKS)----
##pick any 2 continuous variables and  compare them
##comparing ApplicantIncome and LoanAmount
##use the correlation test
cor.test(data5$ApplicantIncome,data5$LoanAmount, method = "pearson",use = "complete obs")
##there is a positive  high correlation
##or using visuals

ggplot(data = data5, mapping = aes(x = ApplicantIncome, y = LoanAmount) + 
  geom_point() +
  labs(x="Income of an Applicant", 
       y="Income of a Coapplicant",
       title="Relationship between the incomes of bank loan applicants and their coapplicants")

  