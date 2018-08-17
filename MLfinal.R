rm(list=ls())
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)

set.seed(12345)
if(!file.exists("pml-training.csv")){
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")}
if(!file.exists("pml-testing.csv")){
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")}

trainData<- read.csv("pml-training.csv", sep=",", header=TRUE, na.strings = c("NA","","#DIV/0!"))
testData<- read.csv("pml-testing.csv", sep=",", header=TRUE, na.strings = c("NA","","#DIV/0!"))
dim(trainData)
dim(testData)

# Cleaning the data
# Remove NearZeroVariance variables
nzv <- nearZeroVar(trainData, saveMetrics=TRUE)
trainData <- trainData[,nzv$nzv==FALSE]
nzv<- nearZeroVar(testData,saveMetrics=TRUE)
testData <- testData[,nzv$nzv==FALSE]

# Partition the trainData into two trainData_TRAIN & trainData_TEST
partition <- createDataPartition(trainData$classe, p=0.6, list=FALSE)
trainData_TRAIN <- trainData[partition, ]
trainData_TEST <- trainData[-partition, ]
dim(trainData_TRAIN)
dim(trainData_TEST)

# Remove first column as it contain serial number.
trainData_TRAIN <- trainData_TRAIN[c(-1)]

# Clean variables with more than 60% NA
cleanV <- trainData_TRAIN
for(i in 1:length(trainData_TRAIN)) {
    if( sum( is.na( trainData_TRAIN[, i])) /nrow(trainData_TRAIN) >= .7){
        for(j in 1:length(cleanV)){
            if( length( grep(names(trainData_TRAIN[i]), names(cleanV)[j])) == 1){
                cleanV <- cleanV[ , -j]
            }   
        } 
    }
}
trainData_TRAIN <- cleanV
rm(cleanV)

# Restructure the trainData_TEST and testData.
temp1 <- colnames(trainData_TRAIN)
temp2 <- colnames(trainData_TRAIN[, -58])
trainData_TEST <- trainData_TEST[temp1]
testData <- testData[temp2]
dim(trainData_TEST)
dim(testData)

# Make all the data into same data type
for (i in 1:length(testData) ) {
    for(j in 1:length(trainData_TRAIN)) {
        if( length( grep(names(trainData_TRAIN[i]), names(testData)[j]) ) == 1)  {
            class(testData[j]) <- class(trainData_TRAIN[i])
        }      
    }      
}
testData <- rbind(trainData_TRAIN[2, -58] , testData)
testData <- testData[-1,]

## Prediction using Decision Trees
set.seed(12345)
DecisionTree <- rpart(classe ~ ., data=trainData_TRAIN, method="class")
fancyRpartPlot(DecisionTree)

Pred_DecisionTree <- predict(DecisionTree, trainData_TRAIN, type = "class")
DTree <- confusionMatrix(Pred_DecisionTree, trainData_TRAIN$classe)
DTree
plot(DTree$table, col = DTree$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(DTree$overall['Accuracy'], 4)))

## Prediction using Random Forests
set.seed(12345)
RandomForest <- randomForest(classe ~ ., data=trainData_TRAIN)
pred_RandomForest <- predict(RandomForest, trainData_TRAIN, type = "class")
RForest <- confusionMatrix(pred_RandomForest, trainData_TRAIN$classe)
RForest
plot(RandomForest)
plot(RForest$table, col = RForest$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(RForest$overall['Accuracy'], 4)))

## Prediction using Generalized Boosted Regression
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
GenBoost <- train(classe ~ ., data=trainData_TRAIN, method = "gbm", trControl = fitControl, verbose = FALSE)
GenBoost_FM <- GenBoost$finalModel

Pred_GenBoost <- predict(GenBoost, newdata=trainData_TRAIN)
Acc_GenBoost <- confusionMatrix(Pred_GenBoost, trainData_TRAIN$classe)
Acc_GenBoost
plot(GenBoost, ylim=c(0.9, 1))

If we compare the above predection based on Accuracy for the Random forest was more accurate then the Decision Trees or GBM.
The expected out-of-sample error is 0 percent.

FinalPred <- predict(RandomForest, testData, type = "class")
FinalPred
