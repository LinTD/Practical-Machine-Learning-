

#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("dplyr")

## Load data sets
setwd("E:\\Coursera\\Practical Machine Learning\\project")
UrlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
UrlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file (UrlTrain, destfile = "training.csv")
download.file (UrlTest, destfile = "testing.csv")

setTrain<-read.table(file="training.csv", header = TRUE, sep = ",")
setTest<-read.table(file="testing.csv", header = TRUE, sep = ",")

##Preprocess data sets
dim(setTrain);dim(setTest)


library(dplyr)
Train<-select(setTrain, contains("accel"),classe)
names(Train)
Test<-select(setTest, contains("accel"))
names(Test)

VarMtest<-which(colSums(is.na(Test))>0)
Test_final<-Test[,-c(VarMtest)]
include<- names(Train) %in% c("classe", names(Test[,-c(VarMtest)])) 
Train_final <- Train[include]

library(caret)
nsv <- nearZeroVar(Train_final, saveMetrics=TRUE)
nsv

dim(Train_final);dim(Test_final)


sapply(Train_final, function(x) sum(is.na(x) | x == ""))
sum(!complete.cases(Train_final))
sapply(Test_final, function(x) sum(is.na(x) | x == ""))
sum(!complete.cases(Test_final))

####split the 'Train' set into 'training' set(60%) and 'testing' set(40%)
set.seed(1968)
library(caret)
inTrain = createDataPartition(y=Train_final$classe, p=0.6, list=FALSE)
training = Train_final[inTrain,]
testing = Train_final[-inTrain,]
dim(training);dim(testing)

summary(training$classe)

##Fit Models
####1. Predicting with trees


library(rattle)
library(rpart.plot)

model<-train(classe~., method="rpart", data=training)
print(model,digits=3)
print(model$finalModel, digits=3)
fancyRpartPlot(model$finalModel)

predictions<-predict(model, newdata=testing)
confusionMatrix(predictions, testing$classe)

library(randomForest)
model2<- randomForest(classe~., data=training)
print(model2,digits=3)
print(model2$finalModel, digits=3)

predictions2<-predict(model2, newdata=testing, type="class")
confusionMatrix(predictions2, testing$classe)

