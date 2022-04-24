library(data.table)
library(caret)
library(Metrics)
library(ggplot2)
library(tidyverse)


# read in training data 
data<-fread('dataset.csv')
head(data)
str(data)
summary(data)
ncol(data)
nrow(data)

# get number of row where target variable is missing
nrow(data[data$Q18a == "",]) 

#Drop rows where NaN appears in the target variable 
data <- data[!(data$Q18a == ""),]

# output number of rows
nrow(data) 


# Drop Unnecessary columns
data <- data[,!c('StartDate', 'EndDate', 'Status', 'IPAddress', 'Progress',
              'Duration (in seconds)', 'Finished', 'RecordedDate', 'ResponseId',
              'RecipientLastName', 'RecipientFirstName', 'RecipientEmail',
              'ExternalReference', 'LocationLatitude', 'LocationLongitude',
              'DistributionChannel', 'UserLanguage', 'Q4', 'Q5', 'Q5_6_TEXT', 'Q7','Q17a', 'Q6','Q29b') ]

#output number of columns 
ncol(data) 

# drop first row which contain the specific question 
data <- data[-1, ]

# output number of missing values in each column 
sapply(data, function(x) sum(x==""))

# drop columns that have more missing values or are irrelevant based on our domain knowledge 
data$Q9b <- NULL
data$Q29c <- NULL
data$Q29d <- NULL
data$Q8a <-NULL
data$Q9a <-NULL
data$Q25a <-NULL



# get unique values in columns and imputate missing values manually with aggregration of levels 


unique(data$Q11a) #how many children at home? ""  "2" "1" "3" "4" "5"
data[data$Q11a == "",]$Q11a <- 0

unique(data$Q11b)
data[data$Q11b == ""]$Q11a
data[data$Q11b == ""]$Q11b <- "not applicable" 

unique(data$Q14a)
data[data$Q14a == ""]$Q14a <- "I am working the same hours"

unique(data$Q15a)
data[data$Q15a =="",]$Q14a
data[(data$Q15a =="") & (data$Q14a == "I am working the same hours")]$Q15a <- "My work schedule has not changed"
data$Q15a <- NULL

unique(data$Q16a)
unique(data$Q16b)
data[data$Q16a ==""]$Q16b


unique(data$Q26a)
unique(data$Q28a)
unique(data$Q29a)

# drop row have more than 6 missing values 
row.has.na <- apply(data, 1,function(x) {sum(x=="") > 6})
data = data[!row.has.na, ]
nrow(data)

# imputate rest of missing values with mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sapply(data, function(x) sum(x==""))


data[data$Q9 ==""]$Q9 <- getmode(data$Q9)
data[data$Q16a ==""]$Q16a <- getmode(data$Q16a)
data[data$Q16b ==""]$Q16b <- getmode(data$Q16b)
data[data$Q26a ==""]$Q26a <- getmode(data$Q26a)
data[data$Q27a ==""]$Q27a <- getmode(data$Q27a)
data[data$Q28a ==""]$Q28a <- getmode(data$Q28a)
data[data$Q29a ==""]$Q29a <- getmode(data$Q29a)



# convert values in target variable to numeric data type 
unique(data$Q18a)
data[data$Q18a == "I am drinking more alcohol",]$Q18a <- 1
data[data$Q18a !=1,]$Q18a <- 0


# convert values in following columns into numeric datatype 
data$Q18a <- as.numeric(data$Q18a)
data$Q1 <- as.numeric(data$Q1)
data$Q11a <- as.numeric(data$Q11a)


# write out the data
fwrite(data,"data_1.csv")
