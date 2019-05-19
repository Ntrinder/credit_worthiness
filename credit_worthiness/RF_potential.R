# -- RANDOM FOREST METHOD --

library(randomForest)

# GET/SET Working Directories
#getwd()
#setwd("D:/College/ds_assignment")
setwd("C:/Users/trind/Documents/01_CIT_DataScience/03_DataScience/05_Assignment/DS_assignment")

# set.seed to last 3 digits of Student No.
set.seed(254)

# Importing Data
past_customers <- read.csv('past_customers.csv', sep = ',')
colnames(past_customers)[1] <- "ID"
potential_customers <- read.csv('potential_customers.csv', sep = ',')
colnames(potential_customers)[1] <- "ID"


# Define test & train sets
train_split = 0.9
s <- sample(1:nrow(past_customers), (nrow(past_customers) * train_split))
PC_train <- past_customers[s,]
PC_test <- past_customers[-s,]

PC_randForest <- randomForest(Credit.Standing~. -ID, data = PC_train, mtry = 6, importance = T)
tuneRF(PC_train[,-14], PC_train[,14], stepfactor=2)
PC_randForest


# Boosted Random Forest
library('gbm')

PC_boosted <- gbm(Credit.Standing~.-ID, data= PC_train, distribution='gaussian', n.trees = 5000, interaction.depth = 3)
yhat_boosted <- predict(PC_boosted, PC_test, n.trees=5000, shrinkage =0.2, verbose=F)
yhat_boosted
