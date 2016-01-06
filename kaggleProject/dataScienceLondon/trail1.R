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
train_input_normalized = normalizeFeatures(train_input)
colnames(train_input_normalized) = c(paste("input_", seq(1, features_input_num), sep=''))
colnames(train_output) = 'output'

# exploratory data
train_data = cbind(train_input_normalized, train_output)

# divide to training data and cross validation data
train_sep = separateTrainCV(train_data)
train_set = train_sep$trainSet
CV_set    = train_sep$CVSet

# implement simple minded neural network
library(neuralnet)
train_colNum = ncol(train_set)

# choose the threshold value by maximizing f score
hidden_nodes = 40
ratio_array = seq(0.05, 0.95, 0.05)
validate_threshold.df = data.frame(threshold=0, fScore=0)
icount = 1
for(threshold_ratio in ratio_array){
  nn.formula =  as.formula(paste(colnames(train_set)[train_colNum],
                                 paste(colnames(train_set)[1:train_colNum-1], collapse='+'),
                                 sep='~')
  )
  # build the model
  nn.model = neuralnet(nn.formula, train_set, 
                       hidden=hidden_nodes, 
                       threshold=0.01,
                       err.fct="sse", linear.output=FALSE)
  nn_cv_test = compute(nn.model, CV_set[,1:train_colNum-1])
  nn_cv_result = nn_cv_test$net.result
  # convert to binary output
  nn_cv_result[(nn_cv_result<threshold_ratio)]=0
  nn_cv_result[(nn_cv_result>=threshold_ratio)]=1  
  # calculate the ratio of wrong classification
  fscore = computefScore(nn_cv_result, CV_set[[train_colNum]])
  validate_threshold.df[icount,1:2] = c(threshold_ratio, fscore)
  icount = icount+1
}

threshold_optimal = validate_threshold.df$threshold[
  which.max(validate_threshold.df$fScore)]

# choose the optimal node number by minimizin error
nodes_array = seq(20, 200, 10)
validate_nodes.df = data.frame(nodeNum=0, misRatio=0)
icount = 1
for(nodes_now in nodes_array){
  nn.formula =  as.formula(paste(colnames(train_set)[train_colNum],
                                 paste(colnames(train_set)[1:train_colNum-1], collapse='+'),
                                 sep='~')
  )
  # build the model
  nn.model = neuralnet(nn.formula, train_set, 
                       hidden=nodes_now, 
                       threshold=0.01,
                       err.fct="sse", linear.output=FALSE)
  nn_cv_test = compute(nn.model, CV_set[,1:train_colNum-1])
  nn_cv_result = nn_cv_test$net.result
  # convert to binary output
  nn_cv_result[(nn_cv_result<threshold_optimal)]=0
  nn_cv_result[(nn_cv_result>=threshold_optimal)]=1  
  # calculate the ratio of wrong classification
  wrongRatio = sum((nn_cv_result-CV_set[train_colNum])^2)/nrow(CV_set)
  validate_nodes.df[icount,1:2] = c(nodes_now, wrongRatio)
  icount = icount+1
}
nodes_optimal = validate_nodes.df$nodeNum[
  which.min(validate_nodes.df$misRatio)]

# make the final model and make prediction
# train the model
nn.formula =  as.formula(paste(colnames(train_set)[train_colNum],
                               paste(colnames(train_set)[1:train_colNum-1], collapse='+'),
                               sep='~')
)
nn.model = neuralnet(nn.formula, train_set, 
                     hidden=nodes_optimal, 
                     threshold=0.01,
                     err.fct="sse", linear.output=FALSE)
# calculate the test result
test_file = file.path(dataFolder, 'test.csv')
test_data = read.csv(test_file, header=F)
colnames(test_data) = c(paste("input_", seq(1, features_input_num), sep=''))
test_data_normalized = normalizeFeatures(test_data)
nn_test = compute(nn.model, test_data_normalized)
nn_test_result = nn_test$net.result
# convert to binary output
nn_test_result[(nn_test_result<threshold_optimal)]=0
nn_test_result[(nn_test_result>=threshold_optimal)]=1 


# make submission data
nn.submissionTest = data.frame(Id=seq(1,nrow(test_data_normalized)), 
                               Solution=nn_test_result)
write.csv(nn.submissionTest, 'testResults_nn.csv',
          row.names=F)
