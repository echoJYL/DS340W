library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ClusterR)
library(tidyverse)


# load in data 
data<-fread("./project/volume/data/raw/data_1.csv")

# we are not supposed to know the party of the individuals so we should hide this
ID<-data$ID
data$ID<-NULL
data$Q18a <- NULL
data$Q3 <- NULL

# do a pca
pca<-prcomp(data)


# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# PC1: 2, 10, 11, 13, 20, 22, 25, PC2:1, 24

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

