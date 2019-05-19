# -- EDA --


# GET/SET Working Directories
#getwd()
#setwd("D:/College/ds_assignment")
setwd("C:/Users/trind/Documents/01_CIT_DataScience/03_DataScience/05_Assignment/DS_assignment")

# set.seed to last 3 digits of Student No.
set.seed(254)

# Importing Data
past_customers <- read.csv('past_customers.csv', sep = ',')
# potential_customers <- read.csv('potential_customers.csv', sep = ',')
# colnames(potential_customers)[1] <- "ID"


# The Credit Standing Variable is the dependant variable
dim(past_customers)
# 780 observations with 14 variables (incl ID)
table(past_customers$Credit.Standing)
# In the past, 461 customers are classified as good credit standing and 319 as bad credit standing


num_complete <-sum(complete.cases(past_customers))
ifelse(num_complete < nrow(past_customers), "Incomplete", "Complete")
# No incomplete or missing variables

# Check the number of cateforical levels, in the past we used
# certain algorithms which limited the number levels it could compute
for(i in 1:ncol(past_customers)){
  column <- colnames(past_customers[i])
  levels <- dim(table(past_customers[,i]))
  print(paste(column, "has", levels, "number of levels"))
}
# ID wont be used in any algorithm but it will remain to
# identify later.
# Residence time and Age are numeric and we will leave them alone for now.

# find if numeric attributes are correlated
#months since opened vs residence
cor(past_customers$Months.since.Checking.Acct.opened, past_customers$Residence.Time..In.current.district.)

#months since opened vs age
cor(past_customers$Months.since.Checking.Acct.opened, past_customers$Age)

# residence vs age
cor(past_customers$Age, past_customers$Residence.Time..In.current.district.)

# low correlations so leave them alone