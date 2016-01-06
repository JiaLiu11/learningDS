# auxiliary functions for machine learning
# Generally they will not depend on other external packages
# unless external packages make the calculation greatly easier.

normalizeFeatures<-function(x){
# normalize the feature vector
  x_normalized = lapply(x, function(xp){(xp-mean(xp))/sd(xp)})
  return(as.data.frame(x_normalized))
}

separateTrainCV<-function(train_data){
# separate the training data set into training data and cross validation
# data set, keep the ratio to be 3:1.
  total_lines = nrow(train_data)
  # trival case
  if(total_lines==1){
    print('only one line of data!')
    train_set = train_data
    CV_set    = train_data
  }else if(total_lines<=3){
    train_set = train_data[1:total_lines-1,]
    CV_set    = train_data[total_lines,]    
  }else{
    # non-trival case
    set.seed(100)
    idx_rand = sample(seq(1, total_lines)) # random index array
    CV_total_lines = as.integer(total_lines/4)    
    CV_idx = idx_rand[1:CV_total_lines]
    train_idx = idx_rand[(CV_total_lines+1):total_lines]
    # divide CV and train set by random indices
    CV_set   = train_data[CV_idx,]
    train_set= train_data[train_idx,]
  }
  row.names(train_set)=NULL
  row.names(CV_set) = NULL
  result_sets = list(trainSet = train_set, CVSet = CV_set)
  return(result_sets)  
}


computefScore<-function(prediction, reality){
# calculate f2 score for skewd data prediction
# input: prediction and reality are both binary vectors
# output: f2 score

  # convert inputs to logical vectors
  prediction_lg = as.logical(prediction)
  reality_lg    = as.logical(reality)
  # find all kinds of errors
  truePositives = sum(prediction_lg*reality_lg)
  falsePositives= sum(prediction_lg*!reality_lg)
  falseNegatives= sum(!prediction_lg*reality_lg)
  # calculate precision and recall
  precision = truePositives/(truePositives+falsePositives)
  recall    = truePositives/(truePositives+falseNegatives)
  # calcualte f score
  fScore = 2.0*precision*recall/(precision+recall)
  return(fScore)
}
