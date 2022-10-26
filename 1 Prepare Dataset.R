library(readr)
features <- read_csv("features.csv")
View(features)
features<-data.frame(features)
features[,1]<-NULL

targets <- read_csv("targets.csv", col_types = cols(...1 = col_skip()))
targets<-data.frame(targets)
targets<-as.vector(targets)

costs <- read_csv("costs.csv")
View(costs)
costs<-t(costs[,2:46])

set.seed(20)

features<-features[targets<2,]
targets<-targets[targets<2]
set.seed(1)
userows<-sample(nrow(features),size=2000)

featuresshort<-features[userows,]
targetsshort<-targets[userows]
rm(features)
rm(targets)
library(dplyr)
featuresshort<-data.frame(sapply(featuresshort,as.numeric))
featuresshort[,which(sapply(featuresshort, n_distinct)<3)]<-sapply(featuresshort[,which(sapply(featuresshort, n_distinct)<3)],as.factor)
library(caret)
set.seed(100)
trainIndex <- createDataPartition(targetsshort, p = .6, 
                                  list = FALSE, 
                                  times = 1)
temp<-setdiff(1:2000,trainIndex)
set.seed(100)
validation<-createDataPartition(targetsshort[temp], p = .4, 
                                     list = FALSE, 
                                     times = 1)
validationIndex<-c(1:2000)[temp][validation]
testIndex<-c(1:2000)[temp][-validation]
