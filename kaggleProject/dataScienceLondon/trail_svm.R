rm(list=ls())

# load necessary functions
source("auxFunctions.R")
library(e1071) # for svm

# read in data
dataFolder = 'data'
train_file = file.path(dataFolder, 'train.csv')
train_result_file = file.path(dataFolder, 'trainLabels.csv')
train_input = read.csv(train_file, header=FALSE)
train_output = read.csv(train_result_file, header=FALSE)

# clean the data
features_input_num  = ncol(train_input)
features_output_num = ncol(train_output)
colnames(train_input) = c(paste("input_", seq(1, features_input_num), sep=''))
colnames(train_output) = 'output'
train_output[['output']] = as.factor(train_output[['output']])

# divide to training data and cross validation data
train_data = cbind(train_input, train_output)
train_colNum = ncol(train_data)
train_sep = separateTrainCV(train_data)
train_set = train_sep$trainSet
CV_set    = train_sep$CVSet

# begin svm to the trainning set:
svm.formula =  as.formula(paste(colnames(train_set)[train_colNum],
                               paste(colnames(train_set)[1:train_colNum-1], 
                                     collapse='+'),
                               sep='~'))
# tune cost function
set.seed(1)
tune.out = tune(svm, svm.formula, data = train_data,
             kernel="radial",
             ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
print(summary(tune.out))
svm.bestmod = tune.out$best.model

print(table(true=train_set[["output"]], 
      pred = predict(svm.bestmod, newdata=train_set[-train_colNum])))

# test svm model against cross validation set
print(table(true=CV_set[["output"]], 
      pred = predict(svm.bestmod, newdata=CV_set[-train_colNum])))

# make submission data
test_file = file.path(dataFolder, 'test.csv')
test_data = read.csv(test_file, header=F)
colnames(test_data) = c(paste("input_", seq(1, features_input_num), sep=''))
test_result = predict(svm.bestmod, newdata=test_data)
svm.submissionTest = data.frame(Id=seq(1,nrow(test_data)), 
                                  Solution=test_result)
write.csv(svm.submissionTest, 'testResults_svm.csv',
          row.names=F)
