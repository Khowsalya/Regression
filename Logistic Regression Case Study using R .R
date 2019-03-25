##############################Logistic Regression##################################
#WorkingDirectory Path Setting

setwd("D:\\Kowsi_DataScience_R\\Class-Datasets")

#Reading given csv file for analysis
Brand  <- read.csv("goodforu-class12 (1).csv", header = T , stringsAsFactors = F)
Brand


#Quick View of data using some functions in R to know some basic details
str(Brand)
dim(Brand)
head(Brand)
tail(Brand)
names(Brand)
summary(Brand)#(data frame of 61 variable and all are int)

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


# Data preparation and exploration steps
#Quick Check Data to identify the column name and purpose description

View(Brand)

#Prepare for Brand A
BrandA <- select(Brand,X2,X9,X16,X23,X30)

#Quick check Brand A
View(BrandA)

summary(BrandA)

#Prepare for meaningful Column names
colnames(BrandA) <- c("Brand_A_ingredients","Brand_A_trans_fat","Brand_A_naturaloils",
                      "Brand_A_good_bad_rating","Brand_A_minimally_Heavilyprocessed")

#X23 Brand A chips : Rate the following 10=good for you, 1=bad for you
# Creating a Column to show Good bad target  with Rate >5 from BrandA

BrandA_GoodBadRatings <- BrandA %>% mutate(Brand_A_Target= ifelse(BrandA$Brand_A_good_bad_rating > 5,1,0))  

#Quick check new column Target

View(BrandA_GoodBadRatings)



#X30 Brand A chips : 10=minimallyProcessed / 1=Heavily processed on a 10 point scale
#Creating a column to show minimally/Heavily processed with rate <= 5 from BrandA_GoodBadRatings

BrandA_GoodBadRatings <- BrandA_GoodBadRatings %>% mutate(BrandA_Target1 = ifelse(BrandA_GoodBadRatings$Brand_A_minimally_Heavilyprocessed <= 5,'Heavily processed','minimally processed'))

#Quick check new column Target1

View(BrandA_GoodBadRatings)
colnames(BrandA_GoodBadRatings)


#Checking missing values
colSums(is.na(BrandA_GoodBadRatings))


#Deleting old columns becoz we have created a new column with meaningful values
BrandA_Final <- BrandA_GoodBadRatings[,-c(4,5)]

#Quick check of New dataset
View(BrandA_Final)
colnames(BrandA_Final)
summary(BrandA_Final)

#splitting dataset into train and test

set.seed(200)
sampling <- sample(nrow(BrandA_Final),0.70*nrow(BrandA_Final), replace = F)

train <-  BrandA_Final[sampling,]
test <- BrandA_Final[-sampling,]

#Checking percentage of goodbad rate column

table(BrandA_Final$Brand_A_Target)/nrow(BrandA_Final)
table(train$Brand_A_Target)/nrow(train)
table(test$Brand_A_Target)/nrow(test)


#Quick check of dataset train and test
View(train)
View(test)


############################################################################

###########################Building model##################################


Reg <- glm( data = train, Brand_A_Target~., family = "binomial")
summary(Reg)

#Found all variables are highly significant
train$predict <- Reg$fitted.values
train$predict




#Finding correlation between numeric variables

library(car)
vif(Reg)

#No multicorrelation found

#Comparing with actual and predicted values 

head(train$Brand_A_Target)

head(train$predict)


#converting  the probabilities into Good/Bad response


#For different cutoff probabilities, the confusion matrix will be different

# To find accuracies for different cut-off probabilities

# There are a lot of performance parameters available in ROCR package
library(ROCR)

pred<-prediction(train$predict,train$Brand_A_Target)

pred

pref <- performance(pred,"acc")

class(pref)

pref



class(pref@x.values)
cutoffprob <- as.numeric(unlist(pref@x.values))

cutoffprob

class(pref@y.values)
accuracies <- as.numeric(unlist(pref@y.values))


cutoffs <- data.frame(cutoffprob, accuracies )


cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

cutoffs

train$predclass1 <- ifelse(train$predict>0.52419126,1,0)

#we got 0.54445567 cutoff prob with the accuracy of  77%

library(caret)
library(irr)
kappa(data.frame(train$Brand_A_Target,train$predclass1))

confusionMatrix(as.factor(train$Brand_A_Target),as.factor(train$predclass1), positive = "1")


#Kappa : 0.3262 and Accuracy : 0.7795
#So It tell that MODEL is Very Good.


#ROCR curve
pref<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(pref,col="red")

abline(0,1, lty = 8, col = "blue")

auc<-performance(pred,"auc")
auc

#The Accuracy is 0.742709; More than 0.50, 


library(gains)
gains(as.numeric(train$Brand_A_Target),train$predict, groups =10)
quantile(train$predict, seq(0,1,0.1))#.1,.2.....,.9,1(decile)

#Top 30% PROBABILITY of customers who thinks BRAND A is Good Lies here 
#(70 to 100%)
#0.25473315 0.42184542 0.54699576 0.64218205 
##Gain Chart tells that top 40% of the Probabilities contacin 70% of customers 
##Likes and believes that "Brand A" Chips are Good

glm(Reg)

#Prediction 1: Are my brands made with farm grown ingredients like potato, corn or wheat?
Q1 <- BrandA_Final%>%group_by(Brand_A_ingredients)%>%summarise(Count=n(),Percent_Count = n()/count(BrandA_Final))%>%ungroup()%>%data.frame()
str(Q1)

#Converting the Brand_A_ingredientds_Target with meaning values(0,1)

Brand_A_ingredients1 <- BrandA%>%mutate(Brand_A_ingredients_Target=ifelse(BrandA$Brand_A_ingredients==1,1,0))
Brand_A_ingredients1<- Brand_A_ingredients1%>%group_by(Brand_A_ingredients_Target)%>%summarise(Count=n(),Percent_Count = n()/count(Brand_A_ingredients1))%>%ungroup()%>%data.frame()
str(Brand_A_ingredients1)

#Close to 80% Customers believe BRAND A Chips are made with farm grown ingredients like potato, corn or wheat.
# Brand_A_ingredients_Target: num  0 1
#Count                     : int  4988 19126
# Percent_Count            :num   0.207 0.793

#Prediction 2:Do my brands have zero grams trans-fat?

Q2 <- BrandA_Final%>%group_by(Brand_A_trans_fat)%>%summarise(Count=n(),Percent_Count = n()/count(BrandA_Final))%>%ungroup()%>%data.frame()
str(Q2)

#Converting the Brand_A_trans_fat with meaningfull values(0,1)

Brand_A_trans_fat1 <- BrandA%>%mutate(Brand_A_trans_fat_Target=ifelse(BrandA$Brand_A_trans_fat==1,1,0))
Brand_A_trans_fat1<- Brand_A_trans_fat1%>%group_by(Brand_A_trans_fat_Target)%>%summarise(Count=n(),Percent_Count = n()/count(Brand_A_trans_fat1))%>%ungroup()%>%data.frame()
str(Brand_A_trans_fat1)

#Close to 32% Customers believe BRAND A Chips have zero grams trans-fat.

#Prediction 3: Are my brands made with natural oils
Q3<- BrandA_Final%>%group_by(Brand_A_naturaloils)%>%summarise(count=n(),Percent_count = n()/count(BrandA_Final))%>%ungroup()%>%data.frame()

str(Q3)
#Converting the Brand_A_naturaloils with meaningfull values(0,1)

Brand_A_naturaloils1 <- BrandA%>%mutate(Brand_A_naturaloils_Target = ifelse(BrandA$Brand_A_naturaloils==1,1,0))
Brand_A_naturaloils1 <- Brand_A_naturaloils1%>%group_by(Brand_A_naturaloils_Target)%>%summarise(count=n(),Percent_count = n()/count(Brand_A_naturaloils1))%>%ungroup()%>%data.frame()
str(Brand_A_naturaloils1)

#close to 45% believe BRAND A made with natural oils

#Prediction 4: Is there an impact due to Processing Level?

BrandA_4OrLessOverall <- BrandA%>%mutate(Brand_A_good_bad_rating_Target =ifelse(BrandA$Brand_A_good_bad_rating<5,1,0))
targetRate_4OrLessOverall <- BrandA_4OrLessOverall %>%group_by(Brand_A_good_bad_rating_Target)%>%summarise(Count=n(),Percent_Count = n()/count(BrandA_4OrLessOverall))%>%ungroup()%>%data.frame()
str(targetRate_4OrLessOverall)

##50% Overall perception is 4 Or 


Q41TargetRate <- BrandA_Final%>%group_by(Brand_A_Target)%>%summarise(Count=n(),Percent_Count = n()/count(BrandA_Final))%>%ungroup()%>%data.frame()
Q42processedRate <- BrandA_Final%>%group_by(BrandA_Target1)%>%summarise(Count=n(),Percent_Count = n()/count(BrandA_Final))%>%ungroup()%>%data.frame()
str(Q41TargetRate)
str(Q42processedRate)


observationGoodBadprocessed <- with(BrandA_Final,table(Brand_A_Target,BrandA_Target1) / nrow(BrandA_Final))
observationGoodBadprocessed
View(observationGoodBadprocessed)


## 62% Customers believe Brand A Chips are Heavily Processed(0) and is Bad(0) for Them
## 11% Customers believe Brand A Chips are Heavily Processed(1) and is Good(1) for Them 
## 13% Customers believe Brand A Chips are Minimally Processed(1) Even they think this is bad(0) for Them   
## 14% Customers believe Brand A Chips are Minimally processed(0) Even they think this is good(1) for Them  


#Count of Customers Believe that Brand A is using farm Grown Ingredients With zero grams transfat

farmGrownIngredientsWithzero_grams_transfat <- BrandA%>%mutate(farmGrownIngredientsWithzero_grams_transfat_TargetRate = ifelse(BrandA$Brand_A_ingredients==1 & BrandA$Brand_A_trans_fat==1,1,0))
farmGrownIngredientsWithzero_grams_transfat <- farmGrownIngredientsWithzero_grams_transfat%>%group_by(farmGrownIngredientsWithzero_grams_transfat_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(farmGrownIngredientsWithzero_grams_transfat))%>%ungroup()%>%data.frame()
str(farmGrownIngredientsWithzero_grams_transfat)
View(farmGrownIngredientsWithzero_grams_transfat)


#Do you believe manufacture X(Which makes BRAND A potato chips) is environmentally responsible? Please rate on a 10 point scale. with 10 being good and 1 being bad



brandAEnvironmentResponse <- select(Brand,X38)
colnames(brandAEnvironmentResponse) <- c("EnvironmentResponse_factor_Rate")
brandAEnvironmentResponse_Summary <- brandAEnvironmentResponse%>%mutate(EnvironmentResponse_factor_TargetRate =ifelse(brandAEnvironmentResponse$EnvironmentResponse_factor_Rate>5,1,0))

S3 <- brandAEnvironmentResponse_Summary%>%group_by(EnvironmentResponse_factor_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(brandAEnvironmentResponse_Summary))%>%ungroup()%>%data.frame()
str(S3)
#less than 50 % of customers believe that manfacture A is environmentally responsible 
#more than 50% believes that the manfacture A is not environmentally responsible

#Is price an important factor in your purchase decision?  Please rate on a 10 point scale. with 10 being good and 1 being bad

brandAPurchaseDecisionPrice <- select(Brand,X48)
colnames(brandAPurchaseDecisionPrice) <- c("PurchaseDecision_factor_Price")
brandAPurchaseDecisionPrice_Summary <- brandAPurchaseDecisionPrice%>%mutate(PurchaseDecision_factor_Price_TargetRate=ifelse(brandAPurchaseDecisionPrice$'PurchaseDecision_factor_Price'>=5,1,0))

S4 <- brandAPurchaseDecisionPrice_Summary%>%group_by(PurchaseDecision_factor_Price_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(brandAPurchaseDecisionPrice_Summary))%>%ungroup()%>%data.frame()
str(S4)

#32 % of customer only who think that price an important factor slightly(=5) in their purchase decision,not important

#Is Brand an important factor in your purchase decision?  Please rate on a 10 point scale. with 10 being good and 1 being bad

brandAPurchaseDecision_Brand <- select(Brand,X41)
colnames(brandAPurchaseDecision_Brand) <- c("PurchaseDecision_factor_Brand")
brandAPurchaseDecisionBrand_Summary <- brandAPurchaseDecision_Brand%>%mutate(PurchaseDecision_factor_Brand_TargetRate=ifelse(brandAPurchaseDecision_Brand$'PurchaseDecision_factor_Brand'>=5,1,0))

S4 <- brandAPurchaseDecisionBrand_Summary%>%group_by(PurchaseDecision_factor_Brand_TargetRate)%>%summarise(Count=n(),Percent_Count = n()/count(brandAPurchaseDecisionBrand_Summary))%>%ungroup()%>%data.frame()
str(S4)

#only 19% of customer thinking brand is an important factor in purchase decision

