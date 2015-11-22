library("downloader")
library(caret)

fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

download.file(fileUrl, dest = "training.csv")

traindf <- read.csv("training.csv")

traindf <- traindf[ , apply(traindf, 2, function(x) {!sum(is.na(x))})]

traindf <- traindf[ , -grep("kurtosis|skewness|max|min|amplitude", names(traindf), ignore.case = TRUE)]

traindf <- traindf[, - c(traindf$user_name, traindf$cvtd_timestamp, traindf$new_window)]

inTrain <- createDataPartition(traindf$classe, p = .7, list = FALSE)

trainingSet <- traindf[inTrain,] 

testingSet <- traindf[-inTrain,]

prCompo <- prcomp(traindf[, -40])

preProdf <- preProcess(traindf[, -40], method="pca", pcaComp=39)

trainedproc <- predict(preProdf, traindf[, -40])

modFit1 <- train(classe~., method="rpart", data=trainingSet)

trainCntrl <- trainControl(method="cv", number = 4, allowParallel=TRUE)

modFit2 <- train(classe~., method="rf", data=trainingSet, trControl=trainCntrl)

modFit3 <- train(classe~., method="gbm", data=trainingSet,trControl = trainCntrl, verbose = FALSE)

pred1 <- predict(modFit1, testingSet)
conf1 <- confusionMatrix(pred1, testingSet$classe)

pred2 <- predict(modFit2, testingSet)
conf2 <- confusionMatrix(pred2, testingSet$classe)

pred3 <- predict(modFit3, testingSet)    
conf3 <- confusionMatrix(pred3, testingSet$classe)

outOfSampleError.accuracy <- sum(pred2 == testingSet$classe)/length(pred2)

fileUrl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(fileUrl2, dest = "testing.csv")

testdf <- read.csv("testing.csv")

answers <- predict(modFit2, testdf)

answers <- c("B", "A", "B", "A", "A", "E", "D", "B", "A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)

head(getTree(modFit2$finalModel, k=2))
datadf <- classCenter(trainingSet[, c(3,4)], trainingSet$classe, modFit2$finalModel)
datadf <- as.data.frame(datadf)
datadf$classe <- rownames(datadf)

classAdfTest <- 
classAdfPred <- 

p <- qplot(pitch_arm, yaw_arm, data=trainingSet)
p + geom_point(aes(pitch_arm, yaw_arm), size=5, shape=4, data=testingSet)

testingSet$predRight <- pred2 == testingSet$classe
qplot(total_accel_arm, total_accel_dumbbell, color=predRight, data=testingSet, main="New Predictions")