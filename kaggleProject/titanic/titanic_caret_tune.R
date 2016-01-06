library(dplyr)
library(caret)
# load data
titanic_train_data = read.csv("data/train.csv", stringsAsFactors=F,
                              na.strings="")
titanic_test_data=read.csv("data/test.csv", stringsAsFactors=F,
                           na.strings="")

# preprocess traning data
titanic_train = dplyr::select(titanic_train_data,
                              Survived, Pclass, Sex,
                              Age, SibSp, Parch, Fare, Embarked)
factorNames = c("Survived", "Pclass", "Sex", "Embarked")
titanic_train[,factorNames] = lapply(titanic_train[,factorNames], as.factor)
titanic_test = dplyr::select(titanic_test_data,
                             Pclass, Sex,
                             Age, SibSp, Parch, Fare, Embarked)
factorNames = c("Pclass", "Sex", "Embarked")
titanic_test[,factorNames] = lapply(titanic_test[,factorNames], as.factor)
# create dummy variables
dummies  = dummyVars(Survived~., data=titanic_train)
titanic_train = predict(dummies, newdata=titanic_train)
dummies  = dummyVars(~.,data=titanic_test)
titanic_test  = predict(dummies, newdata=titanic_test)
# imputation
xTrans = preProcess(titanic_train, method="knnImpute")
train_proc = predict(xTrans, titanic_train)
test_proc  = predict(xTrans, titanic_test)

nzv = nearZeroVar(titanic_train, saveMetrics = T)
nzv[nzv$nzv,]


# train a bagged decision trees
set.seed(100)
ctrl=trainControl(method="cv", number = 10)
treebag = train(train_proc, as.factor(titanic_train_data$Survived),
      method="treebag", trControl = ctrl)
table(truth=titanic_train_data$Survived,
      pred=predict(treebag, newdata=train_proc, type="raw"))

# train a bagged svm model
set.seed(100)
bagctrl=bagControl(fit=svmBag$fit,
                   predict=svmBag$pred, 
                   aggregate=svmBag$aggregate)
svmbag = train(train_proc, as.factor(titanic_train_data$Survived),
               "bag",trControl=ctrl, bagControl=bagctrl)
table(truth=titanic_train_data$Survived,
      pred=predict(svmbag, newdata=train_proc, type="raw"))


# train a random forest, scored 0.78947,  0.74163 if use all 12 features
ctrl = trainControl(method="cv",number=10)
grid_rf = expand.grid(.mtry=c(2,3,4,5,6,8))
set.seed(100)
train_rf = train(train_proc, as.factor(titanic_train_data$Survived),
                 method="rf", metric="Kappa",trControl=ctrl,
                 tuneGrid = grid_rf)
table(truth=titanic_train_data$Survived,
      pred=predict(train_rf, newdata=train_proc))


# train a svm with radial basis, scored 0.78469
ctrl = trainControl(method="cv",number=10)
set.seed(100)
grid_svm = expand.grid(.C=c(0.25, 0.5, 1.0, 1.25, 1.5), 
                       .sigma=c(0.02, 0.06, 0.08,1.0,1.08))
train_svm = train(train_proc, as.factor(titanic_train_data$Survived),
                 method="svmRadial", metric="Kappa",trControl=ctrl,
                 tuneGrid = grid_svm)
table(truth=titanic_train_data$Survived,
      pred=predict(train_svm, newdata=train_proc))

## semi-supervised learning
library(DMwR)
## The user-defined function that will be used in the self-training process
f <- function(m,d) { 
  l <- predict(m,d,type='class')
  c <- apply(predict(m,d,type='prob'), 1, max)
  data.frame(cl=l,p=c)
}

# combine train and test data for semi-supervised learning
test_temp = as.data.frame(cbind(Survived=NA, test_proc))
train_temp = cbind(as.data.frame(as.factor(titanic_train_data$Survived)), 
                   as.data.frame(train_proc))
colnames(train_temp)[1]="Survived"
selfTrain_data = rbind(train_temp,test_temp)

# self train
rfSelfT <- SelfTrain(Survived~.,selfTrain_data,
                     learner('randomForest',list(mtry=4)),'f')
table(truth=train_temp$Survived,
      pred=predict(rfSelfT,train_temp,type='class'))

# generate result
pred = predict(train_svm, newdata=test_proc)

# write to result
result.df = data.frame(PassengerId=titanic_test_data$PassengerId,
                       Survived=as.numeric(levels(pred)[pred]))
write.csv(result.df,
          file="prediction_bagging.csv",
          row.names=FALSE)
