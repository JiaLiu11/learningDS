# PCA analysis to data science London
rm(list=ls())
# libraries
library(GGally)
library(e1071) # for svm

# load data
train_data = read.csv('data/train.csv', header=F)
train_truth= read.csv('data/trainLabels.csv', header=F)
train_truth$V1 = as.factor(train_truth$V1)

# correlation matrix or covariance matrix?
R=cor(train_data)
S=cov(train_data)
sd.train = apply(train_data, 2, sd) #not very similar, so use correlation matrix

# perform PCS using covariance matrix
pca_train = princomp(train_data, cor=S)
loadings(pca_train) # eigen vector in columns
summary(pca_train)

# kaiser's rule: retain the PCs that with larger variance than mean variance
mean(eigen(S)$values) # choose 8
# plot(pca_train, type = "lines")

# train svm
cv.result = data.frame(i=0, correctness=0)
svm.bestmod = NULL
best.ratio = 0
best.length = 0
for(i in 2:40){
  # separate training data and CV data
  train_deduced=cbind(pca_train$scores[,1:i], train_truth)
  colnames(train_deduced)[i+1] = "outcome"
  train_set = train_deduced[1:700,]
  cv_set = train_deduced[701:1000,]
  
  svm.formula =  as.formula(paste(colnames(train_set)[i+1],
                                  paste(colnames(train_set)[1:i], 
                                        collapse='+'),
                                  sep='~'))
  tune.out = tune(svm, svm.formula, data = train_set,
                  kernel="radial",
                  ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
  svm.model = tune.out$best.model
  # cross validation
  cv_pred = predict(svm.model, newdata=cv_set[-(i+1)])
  temp_table = table(pred=cv_pred, truth=train_truth[701:1000,])
  correct_ratio = sum(diag(temp_table))/sum(temp_table)
  if(correct_ratio>best.ratio){
    svm.bestmod=svm.model
    best.ratio=correct_ratio
    best.length=i
  }
  cv.result[i-1,]=c(i, correct_ratio)
  print(c(i, correct_ratio))
}

plot(cv.result, type="l")

# make prediction
test_data = read.csv('data/test.csv',header=F)
S_test = cov(test_data)
pca_test = princomp(test_data, cor=S_test)
test_reduced = pca_test$scores[,1:best.length]
test_result = predict(svm.bestmod, newdata=test_reduced)
svm.submissionTest = data.frame(Id=seq(1,nrow(test_data)), 
                                Solution=test_result)
write.csv(svm.submissionTest, 'testResults_pca_svm.csv',
          row.names=F)
