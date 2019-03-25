

############ Case study on Expenses of person based on some variable#####################

#WorkingDirectory Path Setting

setwd("D:\\Kowsi_DataScience_R\\Class-Datasets")
Expense <- read.csv("expenses.csv", header = T, stringsAsFactors = F)

#Quick View of data using some functions in R to know some basic details

dim(Expense)
str(Expense)
View(Expense)
summary(Expense)

#checking for outliners in dependent values(using histogram and boxplot graph)

hist(Expense$charges)

E1 <- boxplot(Expense$charges)

E1$out

index <- which(Expense$charges %in% E1$out)
index


#Checking outliers for other independent values

E2 <- boxplot(Expense$age)
E3 <- boxplot(Expense$bmi)
E4 <- boxplot(Expense$children)#no outliers in children

E2$out
index1 <- which(Expense$age %in% E2$out)

#Imputing  mean value  with age variable (getting mean value from summary)
Expense$age[index1] = 39.31

E3$out
index2 <- which(Expense$bmi %in% E3$out)

#Imputing mean value with BMI variable (getting mean value from summary)
Expense$bmi[index2] = 30.61


#Finding missing values and imputing with mean value


#checking missing value index
index3 <- which(is.na(Expense$bmi))
index3

Expense$bmi[is.na(Expense$bmi)] <- 30.61

#Install some commonly required package and load the library
#Work with Data Frame and carry out some common manipulation
library(dplyr)


#linear Model or simple regression model function usage

# Gains Chart, Gains tables and lift chart for prediction algorithms
library(gains)

#Kappa matrix; Model Accuracy algorithm 
library(irr) 

#ROCR Curve; Cutoff Parameterized Performance Curve design
library(ROCR)

#Confusion Matrix: Classification and Regression Training 
library(caret)


#Checking correlation between numeric values
 
library(ggplot2)
library(corrplot)

# Bivariate Visualization
with(Expense, qplot(age, charges))
with(Expense, qplot(bmi, charges))
with(Expense, qplot(children, charges))

# Checking correlation values
with(Expense, cor(charges, age))
with(Expense, cor(charges, bmi))
with(Expense, cor(charges, children))


#install.packages("corrplot", dependencies = T)
library(corrgram)

E4 <-cor(Expense[,c(1,3,4,7)])

cormat<-corrgram(E4)
write.csv(cormat,"correlationexpense.csv") 
#therefore no correlation between numeric columns


############## Building models of linear regression #####################################

colnames(Expense)

E5 <- lm(charges ~ age+sex+bmi+children+smoker+region, Expense) 
E5
summary(E5)

step(E5,direction = "backward")

#Gives best fitted model
#To choose a good model

E5 <- lm(formula = charges ~ age + bmi + children + smoker, data = Expense)
summary(E5)


# Creating dummy variables
Expense$Smokeryes <- ifelse(Expense$smoker == "yes",1,0)

#Iteration 1:

E5 <- lm(formula = charges ~ age + bmi + children +Smokeryes , data = Expense)
summary(E5)

#Finding predicted  and resdiual  values
E5$fitted.values
E5$residuals

#creating col for both predicted and resdiual values
Expense$predicted <- E5$fitted.values
Expense$resdiual <- E5$residuals

#Checking correlation between actual and predicted values
qplot(Expense$charges, Expense$predicted)
cor(Expense$charges, Expense$predicted)


# checking resdiual frequency distribution (must be normally distributed,homoscadeasticity)
#Checking for heteroscadeasticity
E6 <- ggplot(Expense, aes( x = as.numeric(rownames(Expense))))
E6 + geom_point(aes (y = Expense$resdiual, color = "blue"))

#Another way of checking normality of residuals
hist(Expense$resdiual)
summary(Expense$resdiual)


#creating Fit Chart

E7 <- data.frame(actual = Expense$charges, pred = Expense$predicted)
E7

str(E7)

#X-axis row number and y-axis (both actual and predicted)
rnum <- as.numeric(rownames(E7))

head(rnum)

E8 <- ggplot(E7, aes(x=rnum,y=actual))
E8+geom_line(color = "blue") +geom_line( data = E7,aes(y=pred),color = "green") 


#Mean Absolute percentage Error(MAPE)#absolute value of resdiual /actual 

Expense$mape <- (abs(Expense$resdiual)/Expense$charges)*100

MAPE <- mean(Expense$mape)
MAPE
#42.0729

colnames(Expense)
E9 <- Expense[,c(7,9,10)]
write.csv(E9,"Mape.csv")
