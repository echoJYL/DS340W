library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ClusterR)
library(tidyverse)


# load in data 
data<-fread("./project/volume/data/raw/clean_data.csv")

# drop column Q3, ID, and target variable (Q3 has a different scale that may intefer PCA & feature importance score proved to be low)
ID<-data$ID
data$ID<-NULL
data$Q18a <- NULL
data$Q3 <- NULL

# do a pca
pca<-prcomp(data)


# look at the percent variance explained by each pca
screeplot(pca)

# see the values of the scree plot in a table 
summary(pca)

# look at the rotation of the variables on the PCs
pca







