
## Dividing the data set into train and test sets
idx <-createDataPartition(titanic_train_data$Survived, p=0.8,list=F)
tr <- train_proc[idx,]
tr_label = as.factor(titanic_train_data[idx,]$Survived)
ts <- train_proc[-idx,]
ts_label = as.factor(titanic_train_data[-idx,]$Survived)

## Learn a tree with the full train set and test it
rf = randomForest(tr, tr_label, mtry=4)
table(predict(rf,ts,type='class'),ts_label)

## Now let us create another training set with most of the target
## variable values unknown
trSelfT = tr
trSelfLabel = tr_label
nas <-createDataPartition(trSelfLabel, p=0.2,list=F)
trSelfLabel[nas] <- NA

## Learn a tree using only the labelled cases and test it
baserf <- randomForest(trSelfT[-nas,],trSelfLabel[-nas],mtry=4)
table(predict(baserf,ts,type='class'),ts_label)

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
selfTran_data = rbind(train_temp,test_temp)

# self train
rfSelfT <- SelfTrain(Survived~.,selfTran_data,
                     learner('randomForest',list(mtry=4)),'f')
table(predict(rfSelfT,train_temp,type='class'),train_temp$Survived)
pred = predict(rfSelfT, test_temp, type='class')

# write to result
result.df = data.frame(PassengerId=titanic_test_data$PassengerId,
                       Survived=as.numeric(levels(pred)[pred]))
write.csv(result.df,
          file="prediction_bagging.csv",
          row.names=FALSE)
