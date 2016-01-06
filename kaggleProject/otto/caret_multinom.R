# load libraries
library(caret)
library(reshape2)

# set to default directory
setwd('~/code/kaggleProject/otto/')

train = read.csv('data/train.csv')
test  = read.csv('data/test.csv')
train$id = NULL
test$id = NULL

# data preprocess 
trainX = train[,names(train)!="target"]

# train control
cvCtrl = trainControl(method="repeatedcv", repeats=3, returnData = F)

# train model
multinomTune = train(trainX, train$target,
                     method="multinom", trControl = cvCtrl)
predLabel = predict(multinomTune$finalModel, type="probs")
confusionMatrix(predLabel, train$target)

# get test result
testLabel = predict(multinomTune$finalModel, test, class="class")
testLabel = data.frame(id=seq(1, length(testLabel)), testLabel)
testLabel = reshape2::dcast(testLabel, id~testLabel)
for(i in 1:nrow(testLabel)){
  testLabel[i, which(is.na(testLabel[i,]))]=0
  testLabel[i, which(testLabel[i,-1]!=0)]=1
}