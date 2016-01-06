# purpose: run penalized SVM
rm(list=ls())

# load libraries
library(penalizedSVM)

# prepare data
train_features = read.csv("data/train.csv", header=F)
train_label    = read.csv("data/trainLabels.csv", header=F)
test_features  = read.csv("data/test.csv", header=F)

# clean data
train_label$V1 = as.factor(train_label$V1)
colnames(train_features) = paste("x",seq(1:40), sep="")
colnames(train_label)    = "y"
colnames(test_features)  = paste("x",seq(1:40), sep="")

# prepare data for training
training    = cbind(train_features, train_label)
svm.formula = as.formula(paste(colnames(training)[41],
                               paste(colnames(training)[1:40], 
                                     collapse='+'),
                               sep='~'))
set.seed(1)
tune.out = tune(svm, svm.formula, data = training,
                kernel="radial",
                ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
print(summary(tune.out))
svm.bestmod = tune.out$best.model
test_pred = predict(svm.bestmod)
print(table(pred = test_pred, true=training[["y"]]))

# prepare for parallel work
svm_predictions = predict(svm.bestmod, newdata = test_features)
svm.submissionTest = data.frame(Id=seq(1,nrow(test_features)), 
                                Solution=svm_predictions)
write.csv(svm.submissionTest, 'testResults_svm_parallel.csv',
          row.names=F)