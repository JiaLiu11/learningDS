rm(list=ls())

# load necessary functions
source("auxFunctions.R")

# read in data
dataFolder = 'data'
train_file = file.path(dataFolder, 'train.csv')
train_result_file = file.path(dataFolder, 'trainLabels.csv')
train_input = read.csv(train_file, header=FALSE)
train_output = read.csv(train_result_file, header=FALSE)

# clean the data
features_input_num  = ncol(train_input)
features_output_num = ncol(train_output)
train_input_normalized = train_input
colnames(train_input_normalized) = c(paste("input_", seq(1, features_input_num), sep=''))
colnames(train_output) = 'output'

# exploratory data
train_data = cbind(train_input_normalized, train_output)

# divide to training data and cross validation data
train_sep = separateTrainCV(train_data)
train_set = train_sep$trainSet
CV_set    = train_sep$CVSet

# begin logistic regression
train_colNum = ncol(train_set)
glm.formula =  as.formula(paste(colnames(train_set)[train_colNum],
                               paste(colnames(train_set)[1:train_colNum-1], collapse='+'),
                               sep='~')
)
mylogit = glm(glm.formula, data=train_set, family=gaussian)
train_predicted = predict(mylogit, type="response")
wrongRatio_train = sum((train_predicted - train_set[train_colNum])^2)/nrow(train_set)


# use less predictors
glm.formula_optimized = as.formula("output~input_1+input_5+input_7+input_15+input_19+
                                   input_23+input_30+input_33+input_34+input_35")
# select threshold
threshold_list = seq(0.05,0.95,0.05)
validate_threshold.df = data.frame(threshold=0, wrongRatio=0)
icount = 1
for(threshold_now in threshold_list){
  mylogit = glm(glm.formula_optimized, data=train_set, family=gaussian)
  cv_predicted = predict(mylogit, CV_set[1:train_colNum-1], type="response")
  cv_predicted[(cv_predicted<=threshold_now)]=0
  cv_predicted[(cv_predicted>threshold_now)]=1
  wrongRatio_CV = sum((cv_predicted - CV_set[train_colNum])^2)/nrow(CV_set)
  validate_threshold.df[icount,] = c(threshold_now, wrongRatio_CV)
  icount=icount+1
}

threshold_optimized = validate_threshold.df$threshold[
  which.min(validate_threshold.df$wrongRatio)]


# model for prediction
mylogit_optimized = glm(glm.formula_optimized, data=train_set, family=gaussian)
# calculate the test result
test_file = file.path(dataFolder, 'test.csv')
test_data = read.csv(test_file, header=F)
colnames(test_data) = c(paste("input_", seq(1, features_input_num), sep=''))
logit_test_result = predict(mylogit_optimized, test_data, type="response")
# convert to binary output
logit_test_result[(logit_test_result<=threshold_optimized)]=0
logit_test_result[(logit_test_result>threshold_optimized)]=1

# make submission data
logit.submissionTest = data.frame(Id=seq(1,nrow(test_data)), 
                               Solution=logit_test_result)
write.csv(logit.submissionTest, 'testResults_logit.csv',
          row.names=F)
