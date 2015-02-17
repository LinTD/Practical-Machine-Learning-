

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
Train<-select(setTrain, contains("accel"),raw_timestamp_part_1,raw_timestamp_part_2, classe)
names(Train)
Test<-select(setTest, contains("accel"),raw_timestamp_part_1,raw_timestamp_part_2)
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
####1. Predicting with trees using 'rpart'
library(rattle)
library(rpart.plot)

model<-train(classe~., method="rpart", data=training)
print(model,digits=3)
print(model$finalModel, digits=3)
fancyRpartPlot(model$finalModel)

####2. Predicting with random forest
library(randomForest)
model2<- randomForest(classe~., data=training,proximity=TRUE)
print(model2,digits=3)



##Model evaluation
####Model1 (method="rpart")
predictions<-predict(model, newdata=testing)
confusionMatrix(predictions, testing$classe)

####Model2 (method="randomForest")
predictions2<-predict(model2, newdata=testing, type="class")
confusionMatrix(predictions2, testing$classe)




# check if a data folder exists; if not then create one
if (!file.exists("data")) {dir.create("data")}
setwd("E:\\Coursera\\Practical Machine Learning\\project\\data")

predictTest <- predict(model2, newdata=Test_final)
predictTest

answers = rep("A", 20)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictTest)

