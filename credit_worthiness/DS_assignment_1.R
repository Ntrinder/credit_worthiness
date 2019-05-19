
# Author:               Niall Trinder
# Student Number:       R00088254
# Subject:              DATA8001 Data Science & Analytics
# Date:                 November 2018


# GET/SET Working Directoriesm as required
# getwd()
setwd("C:/Users/trind/Documents/01_CIT_DataScience/03_DataScience/05_Assignment/DS_assignment")

# Load in libraries
library(tree)
library(randomForest)
library(gbm)
library(ROCR)
library(dplyr)
library(magrittr)

# accuracy function 
getAccuracy <- function(table){
  print(table[5] / table[8]) # bad
  print(table[1] / table[7]) # good
  print(table[1] + table[5]) # overall
}


# Set seed as last 3 digits of Student Number
set.seed(254)

# Load in data and rename columns
past_customers <- read.csv('past_customers.csv', sep = ',', na.strings = c("", "NA"))
potential_customers <- read.csv('potential_customers.csv', sep = ',', na.strings = c("", "NA"))

# merging because levels are screwed otherwise
data_merge <- merge(past_customers, potential_customers, all= T)

# split the data again based on where credit standing = na
past_customers <- data_merge[!is.na(data_merge$Credit.Standing),]
potential_customers <- data_merge[is.na(data_merge$Credit.Standing),]

colnames(past_customers)[1] <- "ID"
colnames(past_customers)[11] <- "Opened"
colnames(past_customers)[12] <- "Residence"
colnames(potential_customers)[1] <- "ID"
colnames(potential_customers)[11] <- "Opened"
colnames(potential_customers)[12] <- "Residence"

# remove na
past_customers <- na.omit(past_customers)

# Define test & train sets, 30-70 was found to be a good split
train_split = 0.70
s <- sample(1:nrow(past_customers), (nrow(past_customers) * train_split))
past_train <- past_customers[s,]
past_test <- past_customers[-s,]



# |--------  Build decision tree using training data  -------|>

tree_model <- tree(Credit.Standing~., past_train[,c(-1,-12)])
tree_predict <- predict(tree_model, past_test[,-1], type = 'class')
table_tree <- table(tree_predict, past_test$Credit.Standing) %>%
                    prop.table %>%
                    round(3) %>%
                    addmargins
getAccuracy(table_tree)
summary(tree_model)


# Cross Validation
# We want to perform 10-fold CV on this tree model.
# The cv.tree will use 90% of the training data to
# create the model and 10% to test. This is repeated
# 10 times (10 fold). This also generates a plot which
# graphs deviance vs no of nodes. We can use this
# as a tuning parameter.
# 
# Ref http://gsp.humboldt.edu/OLM/R/05_04_CART.html

tree_cv <- cv.tree(tree_model)
# graph the result of cv to decide how to prune tree
jpeg("tree_cv.jpg")
plot(tree_cv$size, tree_cv$dev, type='b')
dev.off()

# Deviance reducing to ~4 where dimishing returns kicks in.
# Tune/prune model by adjusting number of nodes
tree_model <- prune.tree(tree_model, best=4)
# Make predictions on test set then compare it to the label
tree_predict <- predict(tree_model, past_test[,c(-1,-12)], type = 'class')
table_tree <- table(tree_predict, past_test$Credit.Standing) %>%
                    prop.table %>%
                    round(3) %>%
                    addmargins
getAccuracy(table_tree)
summary(tree_model)

# making a table to keep track of accuracy
accuracy <- data.frame(model=c("Tree"), accuracy=c(table_tree[1]+table_tree[5]),
                       good=table_tree[5] / table_tree[8], bad = table_tree[1] / table_tree[7])

# Graphical Output of Model.
jpeg("tree_model.jpg")
plot(tree_model)
text(tree_model, pretty = 0)
dev.off()

# Apply model to potential customers!
tree_predictions <- predict(tree_model, potential_customers[,c(-1,-12)], type='class')

# Create new data set with the given customer assessments.
assessed_customers <- cbind(potential_customers[,-14], Assessment=tree_predictions)
write.csv(assessed_customers, file="AssessedCustomers_tree.csv")


# |-----------------   NOTES    ------------------|>
# save as csv to import to rmd
# What are our tuning parameters
# - test/train ratio 
# - no of nodes on tree
# - cost matrix?
# Performance Metric?
# - ROC curve, AUC?
# - Accuracy?
# Look at confusion matrix




# |-------    Build random forest model   --------|>
set.seed(254)
RF_model <- randomForest(Credit.Standing~.-ID-Residence, data = past_train, mtry = 6, importance = T, ntree=500)
jpeg("tuneRF.jpg")
tuneRF(past_train[,-14], past_train[,14], stepfactor=1)
dev.off()
# choose mtry = 6 and feed back into model above

RF_predict <- predict(RF_model, past_test, type='class')

table_RF <- table(RF_predict, past_test$Credit.Standing) %>%
            prop.table %>%
            round(3) %>%
            addmargins
getAccuracy(table_RF)

accuracy_RF <- data.frame(model=c("RF"), accuracy=c(table_RF[1]+table_RF[5]),
                          good=table_RF[5] / table_RF[8], bad = table_RF[1] / table_RF[7])
accuracy <- rbind(accuracy, accuracy_RF)

# Compare random forst to decision tree for potential customers
RF_predictions <- predict(RF_model, potential_customers, type = 'class')
View(RF_predictions)
# same as decision tree




# apply stratified sampling
set.seed(254)
RF_sModel <- randomForest(Credit.Standing~.-ID-Residence, data = past_train, mtry = 3, 
                          importance = T, ntree=500, sampsize = c(50, 250), strata=past_train$Credit.Standing)
tuneIT <- tuneRF(past_train[,-14], past_train[,14], stepfactor=1)
RF_sPredict <- predict(RF_sModel, past_test, type='class')

table_sPredict <- table(RF_sPredict, past_test$Credit.Standing) %>%
                  prop.table %>%
                  round(3) %>%
                  addmargins
getAccuracy(table_sPredict)

summary(RF_sModel)
RF_sModel$confusion

RF_sPrediction <- predict(RF_sModel, potential_customers, type='class')
View(RF_sPrediction)
write.csv(RF_sPrediction, file="Assessment_CostAnalysis.csv")
# this predicts more customers to have good credit standing
# it reduces the number of false positives at the expense of
# basically all others.



# |-------------    Build GBM model   ------------|>

# found it 'best' to change label to binary & seperate the data from the label
# past_train$Credit.Standing <- ifelse(past_train$Credit.Standing=="Good",1,0)
# credit_standing <- past_train$Credit.Standing
# train <- past_train[,c(-4,-14)]
# test <- past_test[,c(-4,-14)]
# 
# model <- gbm.fit(x=past_train[,c(-4,-14)], 
#                  y=credit_standing, distribution = 'bernoulli', 
#                  n.trees=1000, 
#                  shrinkage = 0.01,
#                  interaction.depth = 2,
#                  n.minobsinnode=10)
# 
# testpredict <- predict(object=model, newdata = past_test[,c(-4,-14)], n.trees = 1000, type = "response")
# testpredict <- round(testpredict)
# testpredict <- ifelse(testpredict==1, "Good", "Bad")
# table(testpredict, past_test$Credit.Standing)
# predictions <- predict(object=model, newdata = potential_customers[,c(-4,-14)], n.trees = 1000, type = 'response')

past_train$Credit.Standing <- ifelse(past_train$Credit.Standing=="Bad",0,1)
gbm_model <- gbm(Credit.Standing~.-ID-Residence, data = past_train, distribution='bernoulli', 
                 shrinkage = 0.005, n.trees = 2000, interaction.depth = 1, cv.folds = 5)
gbm_model

gbm_predict_train <- predict(gbm_model, past_train[,-14], n.trees=2000, type='response')
gbm_predict_train <- round(gbm_predict_train)
gbm_predict_train <- ifelse(gbm_predict_train==0, "Bad", "Good")
table(gbm_predict_train, past_train$Credit.Standing)

# predict on test
gbm_predict <- predict(gbm_model, past_test[,-14], n.trees=2000, type='response')
gbm_predict <- round(gbm_predict)
gbm_predict <- ifelse(gbm_predict==0, "Bad", "Good")
table(gbm_predict, past_test$Credit.Standing)

table_gbm <- table(gbm_predict, past_test$Credit.Standing) %>%
  prop.table %>%
  round(3) %>%
  addmargins
getAccuracy(table_gbm)
accuracy_gbm <- data.frame(model=c("RF"), accuracy=c(table_gbm[1]+table_gbm[5]),
                             good=table_gbm[5] / table_gbm[8], bad = table_gbm[1] / table_gbm[7])
accuracy <- rbind(accuracy, accuracy_gbm)



gbm_predictions <- predict(gbm_model, potential_customers, n.trees=2000, type='response')
gbm_predictions <- round(gbm_predictions)
gbm_predictions <- ifelse(gbm_predictions==0, "Bad", "Good")
View(gbm_predictions)
# Very questionable predictions? Check if "0", "Bad" & "Good" are correct from above

# ref https://www.r-bloggers.com/using-a-gbm-for-classification-in-r/






# -------------- FINDING SUSPICOUS ENTRIES ------------>

# get ID, RF answers and actual answers, sort by ID, compare RF vs actual, find inconsistancy
# na omited, just have to get ov erhtat
past_train$Credit.Standing <- ifelse(past_train$Credit.Standing=="Bad", 1, 0)
data_merge <- merge(past_test, past_train, all = T)
RF_pre <- predict(RF_model, data_merge, type='class')
tree_pre <- predict(tree_model, data_merge, type='class')
compare <- cbind(data_merge[,c(1,14)], tree_pre)
write.csv(compare, file="CompareTree.csv")

ID <- NULL
count <- 0
longest <- 0
for (i in 1:nrow(compare)){
  if(compare[i,2] != compare[i,3]){
    count = count + 1
  } else {
    count = 0
  }
  if(count > longest){
    longest <- count
    ID <- compare[i,1]
  }
}
ID #?
longest

# when analysed in excel, it would appear that from id 305 to 325 are suspicious
# as they consisntantly disagree with the model.
# lets remove these and see what effect that has on the tree model.

# This should have been done in another script because theres too much overlap going on
past_customers <- read.csv('past_customers.csv', sep = ',', na.strings = c("", "NA"))
potential_customers <- read.csv('potential_customers.csv', sep = ',', na.strings = c("", "NA"))

# merging because levels are screwed otherwise
data_merge <- merge(past_customers, potential_customers, all= T)

# split the data again based on where credit standing = na
past_customers <- data_merge[!is.na(data_merge$Credit.Standing),]
potential_customers <- data_merge[is.na(data_merge$Credit.Standing),]

# remove the suspect entries
past_customers_rm <- past_customers[-305:-325,]

set.seed(254)

# Define test & train sets
train_split = 0.70
s <- sample(1:nrow(past_customers_rm), (nrow(past_customers_rm) * train_split))
past_train_rm <- past_customers_rm[s,]
past_test_rm <- past_customers_rm[-s,]

# repeating tree model to view differences
tree_model_rm <- tree(Credit.Standing~., past_train_rm[,-1])
tree_predict_rm <- predict(tree_model_rm, past_test_rm[,-1], type = 'class')
table_tree_rm <- table(tree_predict_rm, past_test_rm$Credit.Standing) %>%
  prop.table %>%
  round(3) %>%
  addmargins
getAccuracy(table_tree_rm)
summary(tree_model_rm)

tree_cv_rm <- cv.tree(tree_model_rm)
plot(tree_cv_rm$size, tree_cv_rm$dev, type='b')

# Deviance reducing to ~4 where dimishing returns kicks in.
# Tune/prune model by adjusting number of nodes
tree_model_rm <- prune.tree(tree_model_rm, best=4)
tree_predict_rm <- predict(tree_model_rm, past_test_rm[,-1], type = 'class')
table_tree_rm <- round(prop.table(table(tree_predict_rm, past_test_rm$Credit.Standing)),3)
table_tree_rm <- addmargins(table_tree_rm)
summary(tree_model_rm)
table_tree[5] / table_tree[8] # good
table_tree[1] / table_tree[7] # bad
table_tree[1] + table_tree[5] # overall 

# Graphical Output of Model.
plot(tree_model_rm)
text(tree_model_rm, pretty = 0)

# Apply model to potential customers!
tree_predictions_rm <- predict(tree_model_rm, potential_customers[,-1], type='class')
tree_predictions_rm





# |-------------    Performance Analysis <<< SECTION REMOVED!! >>>   ------------|>
 
# n_trees = seq(from=100, to=5000, by=100)
# prediction_matrix <- predict(gbm_model, past_train, n.trees = n_trees)
# dim(prediction_matrix)
# scores <- NULL
# for(i in 1:50) {
# pred_4 <- prediction(prediction_matrix[,i], past_train[,14])
# perf_4 <- performance(pred_4, "tpr", "fpr")
# auc <- performance(pred_4, 'auc')@y.values[[1]]
# scores <- cbind(scores, auc)
# }
# plot(y = scores, x=1:50, type='o')
# plot(perf_4, col='green', lwd=2)
# 
# #ref https://datascienceplus.com/gradient-boosting-in-r/
# 
# # Model Comparison
# comparison_legend <- c("Decision Tree, auc=", "Random Forest, auc=", "Boosted Random Forest, auc=")
# 
# 
# pred_2 <- prediction(RF_predictions[,2], past_test[,14])
# perf_2 <- performance(pred_2, "tpr", "fpr")
# plot(perf_2, col='blue', lwd=2, add=T)
# 
# pred_3 <- prediction(bRF_predictions, past_test[,14])
# perf_3 <- performance(pred_3, "tpr", "fpr")
# plot(perf_3, col='green', lwd=2)
# 
# 
# # Model with missclassification costs
# # boost the number of bad credit rating?
# # Introduce cost matrix (minimal effect)
# 
# costMatrix <- matrix(c(0,5,1,0), nrow=2)
# RF_costModel <- randomForest(Credit.Standing~. -ID, data = past_train, mtry = 6, importance = T, parms = list(loss = costMatrix))
# 

# Identify suspicious or incorrect pattern
