install.packages("randomForest")
install.packages("MASS")
library(randomForest)
library(MASS)
library(caret)

# USe the set.seed function so that we get same results each time 
set.seed(123)
Com_Data <- read.csv(file.choose())
str(Com_Data)
Com_Data$ShelveLoc<-as.factor(Com_Data$ShelveLoc)
Com_Data$Urban<-as.factor(Com_Data$Urban)
Com_Data$US<-as.factor(Com_Data$US)
hist(CompanyData$Sales)

high_sales = ifelse(Com_Data$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(Com_Data[2:11], high_sales)
str(CD)
table(CD$high_sales)
CD$high_sales<-as.factor(CD$high_sales)


# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(high_sales~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting


# each tree node.
# Out of bag estimate of error rate is 16.84 % in Random Forest Model.
attributes(rf)


# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)

head(train$high_sales)

# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$high_sales)   # 100 % accuracy on training data 


# more than 95% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$high_sales) # 84.35 % accuracy on test data 

# Error Rate in Random Forest Model :
plot(rf)

rf1 <- randomForest(high_sales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred3 <- predict(rf1, train)
confusionMatrix(pred1, train$high_sales)  # 100 % accuracy on training data 


# Around 98% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# test data prediction using the Tuned RF1 model
pred4 <- predict(rf1, test)
confusionMatrix(pred4, test$high_sales) # 84.35 % accuracy on test data 

# Confidence Interval is around 90 % 


# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees")
# Majority of the trees has an average number of 45 to 50 nodes. 
# Variable Importance :
varImpPlot(rf1)


# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say ShelveLoc is the most important variable for prediction.on looking at population,it has no value.
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Price is very important and Urban is not that important.
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
