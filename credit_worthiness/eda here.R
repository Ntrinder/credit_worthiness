"""
a) Exploratory Data Analysis (EDA): 
    Carry out some EDA on the data set; 
    carry out at least one trivariate analysis; 
    do you notice anything unusual or any patterns with the data set? 
      Detail these and outline any actions you propose to take 
      before you start model building in part c).
Max word count 500 words.
"""
library(ggplot2)

# Load in data and rename columns
past_customers <- read.csv('past_customers.csv', sep = ',', na.strings = c("", "NA"))
potential_customers <- read.csv('potential_customers.csv', sep = ',', na.strings = c("", "NA"))

colnames(past_customers)[1] <- "ID"
colnames(past_customers)[11] <- "Opened"
colnames(past_customers)[12] <- "Residence"
colnames(potential_customers)[1] <- "ID"
colnames(potential_customers)[11] <- "Opened"
colnames(potential_customers)[12] <- "Residence"

# Grouped Bar Plots
num_var <- (length(names(past_customers)) - 1)
for(i in names(past_customers)){
  counts <- table(past_customers$Credit.Standing, past_customers[,i+1])
  name <- paste0(i,'.jpg')
  jpeg(name)
  barplot(counts, main=i,
          xlab=i, col=c("darkblue","red"),
          legend= rownames(counts), beside=F)
  dev.off()
}

dim(past_customers)
# 780 observations with 14 variables (incl ID)

table(past_customers$Credit.Standing)
# In the past, 461 customers are classified as good credit standing and 319 as bad credit standing

summary(past_customers)

num_complete <-sum(complete.cases(past_customers))
ifelse(num_complete < nrow(past_customers), "Incomplete", "Complete")

# Shapiro tests on numericals
shapiro.test(past_customers$Opened)
shapiro.test(past_customers$Age)
shapiro.test(past_customers$Residence)


# Check the number of cateforical levels, in the past we used
# certain algorithms which limited the number levels it could compute
for(i in 1:ncol(past_customers)){
  column <- colnames(past_customers[i])
  levels <- dim(table(past_customers[,i]))
  print(paste(column, "has", levels, "number of levels."))
}

# find if numeric attributes are correlated
# opened vs residence
cor.test(past_customers$Opened, past_customers$Residence)

# opened vs age
cor.test(past_customers$Opened, past_customers$Age)

# residence vs age
cor.test(past_customers$Age, past_customers$Residence)

# multivariate
# plot age vs opened vs credit standing

jpeg("tri_plot.jpg")
ggplot(past_customers, aes(x = Age, y = Opened)) + geom_point(
  colour = ifelse(past_customers$Credit.Standing=="Good", "blue","red"))
dev.off()
  
                                                              