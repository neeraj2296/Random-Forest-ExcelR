#Importing Necassary Packages/Libraries
library(randomForest)
library(MASS)
library(caret)

#loading the data set 
# USe the set.seed function so that we get same results each time 
set.seed(123)
Fraud <- read.csv(file.choose())
#Factorising the character variables as categories.
Fraud$Undergrad<-as.factor(Fraud$Undergrad)
Fraud$Marital.Status<-as.factor(Fraud$Marital.Status)
Fraud$Urban<-as.factor(Fraud$Urban)
hist(Fraud$Taxable.Income)#Visualizing the Taxable Income Data set

Risky = ifelse(Fraud$Taxable.Income<= 30000, "Risky", "Not Risky")# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(Fraud,Risky)
FC = FCtemp[,c(1:7)]
FC$Risky<-as.factor(FC$Risky)#Factorising the Risky column
str(FC)
table(FC$Risky)   # 476 non risky customers and 124 risky customers


# Data is Partitioned as train and test set 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting


# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky)

# looks like the first six predicted value and original value matches.
confusionMatrix(pred1, train$Risky)   # 100 % accuracy on training data 


# more than 98% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky) # 100 % accuracy on test data 

# Error Rate in Random Forest Model :
plot(rf)

#Training the train set using Random forest Model.
rf1 <- randomForest(Risky~., data=train, ntree = 200, mtry = 2, importance = TRUE,proximity = TRUE)
rf1

#Predicting 
pred3 <- predict(rf1, train)
confusionMatrix(pred3, train$Risky)  # 100 % accuracy on training data 


# Around 99% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 
# test data prediction using the Tuned RF1 model
pred4 <- predict(rf1, test)
confusionMatrix(pred4, test$Risky) # 100 % accuracy on test data 

# Confidence Interval is around 97 % 
# no of nodes of trees
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

# Majority of the trees has an average number of more than 80 nodes. 
# Variable Importance :
varImpPlot(rf1)

# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say Taxable.Income is the most important variable for prediction.on looking at City population,it has no value.
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Taxable.Income is very important and Urban is not that important.
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
