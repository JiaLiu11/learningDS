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
preProcValues = preProcess(trainX, method=c("center", "scale"))
scaledTrain = predict(preProcValues, trainX)

# train control
cvCtrl = trainControl(method="repeatedcv", repeats=3, returnData = F)

# train tree model
set.seed(1)
rpartTune = train(scaledTrain, train$target, 
                method="rpart", trControl = cvCtrl,
                )
rpartPred = predict(rpartTune$finalModel, type="class")
# print(confusionMatrix(rpartPred, train$target))

# train boost model
grid = expand.grid(.model = "tree", .trials=c(1:100), .winnow=FALSE)
set.seed(1)
c5Tune = train(trainX, train$target,
               method="C5.0",
               tuneGrid=grid)

# compare models
cvValues=resamples(list(CART=rpartTune, C5.0=c5Tune))
summary(cvValues)
dotplot(cvValues)


