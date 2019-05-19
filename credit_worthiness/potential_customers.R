library(tree)
# GET/SET Working Directories
#getwd()
#setwd("D:/College/ds_assignment")

# set.seed to last 3 digits of Student No.
set.seed(254)

# Importing Data
past_customers <- read.csv('Credit_Risk5_final.csv', sep = ',')
colnames(past_customers)[1] <- "ID"
potential_customers <- read.csv('potential_customers.csv', sep = ',')
colnames(potential_customers)[1] <- "ID"

# Define test & train sets
train_split = 0.9
s <- sample(1:nrow(past_customers), (nrow(past_customers) * train_split))
PC_train <- past_customers[s,]
PC_test <- past_customers[-s,]

# Build decision tree using training data
PC_tree <- tree(Credit.Standing~.-ID, PC_train)
summary(PC_tree)

# Cross Validation
# We want to perform 10-fold CV on this tree model.
# The cv.tree will use 90% of the training data to
# create the model and 10% to test. This is repeated
# 10 times (10 fold). This also generates a plot which
# graphs deviance vs no of nodes. We can use this
# as a tuning parameter.

# Ref http://gsp.humboldt.edu/OLM/R/05_04_CART.html

PC_cv <- cv.tree(PC_tree)
plot(PC_cv$size, PC_cv$dev, type='b')

# Deviance reducing to ~6 where dimishing returns kicks in.
# Tune model
PC_model <- prune.tree(PC_tree, best=6)

# Graphical Output of Model.
plot(PC_model)
text(PC_model, pretty = 0)


# Predict using model on the test data set asside at the start.
y_hat <- predict(PC_model, PC_test)

potential_predict <- round(predict(PC_model, potential_customers),3)
potential_customers <- cbind(potential_customers, potential_predict)
View(potential_customers)
#PC_model -> text output

# NOTES
# save as csv to import to rmd
# What are our tuning parameters? test/train ratio, no of nodes on tree.
# HAVE A LOOK INTO CONFUSION MATRIX AND ALSO ROC CURVE