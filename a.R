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

nrow(data[data$Q18a == "",]) #635

#Drop rows where NaN appears in "Please tell us how your mood has changed.  My mood has been:" column because it is going to be our target 

data <- data[!(data$Q18a == ""),]

nrow(data) #280


# Drop Unnecessary columns
data <- data[,!c('StartDate', 'EndDate', 'Status', 'IPAddress', 'Progress',
              'Duration (in seconds)', 'Finished', 'RecordedDate', 'ResponseId',
              'RecipientLastName', 'RecipientFirstName', 'RecipientEmail',
              'ExternalReference', 'LocationLatitude', 'LocationLongitude',
              'DistributionChannel', 'UserLanguage', 'Q4', 'Q5', 'Q5_6_TEXT', 'Q7','Q17a', 'Q6','Q29b') ]

ncol(data) # 42
data <- data[-1, ]

sapply(data, function(x) sum(x==""))


data$Q9b <- NULL
data$Q29c <- NULL
data$Q29d <- NULL

unique(data$Q8a)
unique(data$Q9a)

unique(data$Q11a) #gow many children at home? ""  "2" "1" "3" "4" "5"
data[data$Q11a == "",]$Q11a <- 0

unique(data$Q11b)
data[data$Q11b == ""]$Q11a
data[data$Q11b == ""]$Q11b <- "not applicable" 

unique(data$Q14a)
data[data$Q14a == ""]$Q14a <- "I am working the same hours"

unique(data$Q15a)
data[data$Q15a =="",]$Q14a
data[(data$Q15a =="") & (data$Q14a == "I am working the same hours")]$Q15a <- "My work schedule has not changed"



unique(data$Q16a)
unique(data$Q16b)

data[data$Q16a ==""]$Q16b

unique(data$Q25a)
unique(data$Q26a)
unique(data$Q28a)
unique(data$Q29a)



row.has.na <- apply(data, 1,function(x) {sum(x=="") > 6})

data = data[!row.has.na, ]

nrow(data)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



sapply(data, function(x) sum(x==""))


#data[data$Q8a ==""]$Q8a <- getmode(data$Q8a)
data[data$Q9 ==""]$Q9 <- getmode(data$Q9)
#data[data$Q9a ==""]$Q9a <- getmode(data$Q9a)
data[data$Q15a ==""]$Q15a <- getmode(data$Q15a)
data[data$Q16a ==""]$Q16a <- getmode(data$Q16a)
data[data$Q16b ==""]$Q16b <- getmode(data$Q16b)
#data[data$Q25a ==""]$Q25a <- getmode(data$Q25a)
data[data$Q26a ==""]$Q26a <- getmode(data$Q26a)
data[data$Q27a ==""]$Q27a <- getmode(data$Q27a)
data[data$Q28a ==""]$Q28a <- getmode(data$Q28a)
data[data$Q29a ==""]$Q29a <- getmode(data$Q29a)

data$Q8a <-NULL
data$Q9a <-NULL
data$Q25a <-NULL




sapply(data, function(x) sum(x==""))
sapply(data, function(x) sum(is.na(x)))

unique(data$Q18a)
data[data$Q18a == "I am drinking more alcohol",]$Q18a <- 1
data[data$Q18a !=1,]$Q18a <- 0


data$Q18a <- as.numeric(data$Q18a)


dummies <- dummyVars(Q18a ~ ., data = data)
data<-predict(dummies, newdata = data)


data$Q1 <- as.numeric(data$Q1)
data$Q11a <- as.numeric(data$Q11a)


fwrite(submission_6,"./project/volume/data/processed/submit_xgb_2.csv") 
fwrite(data,"data_1.csv")
