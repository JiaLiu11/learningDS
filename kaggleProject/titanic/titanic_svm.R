# train svm model to "predict" survival passengers

library(data.table)
library(Amelia)
library(plyr)
library(e1071) # load svm
library(dplyr)

# load data
titanic_train_data = fread("data/train.csv", na.strings="")
titanic_test_data=fread("data/test.csv", na.strings="")


# clean data
# train data
titanic_train_data$Survived = factor(titanic_train_data$Survived, 
                                     levels=c(0, 1),
                                     labels=c("0","1")) 
titanic_train_data$Pclass   = factor(titanic_train_data$Pclass,
                                     levels=c(1, 2, 3),
                                     labels=c("1","2","3"))
titanic_train_data$Sex      = factor(titanic_train_data$Sex, 
                                     levels=c("female", "male"),
                                     labels=c("0", "1"))# for easy interpretation
titanic_train_data$Embarked = as.factor(titanic_train_data$Embarked)
# test data
titanic_test_data$Pclass = factor(titanic_test_data$Pclass,
                                  levels=c(1, 2, 3),
                                  labels=c("1","2","3"))
titanic_test_data$Sex = factor(titanic_test_data$Sex, 
                               levels=c("female", "male"),
                               labels=c("0", "1"))# for easy interpretation

# impute train data
a.out = amelia(titanic_train_data, m=5, 
               idvars = c(2, 3, 4, 5, 9,11,12),
               p2s=0) # exclude factors, make 5 imputed datasets
train_data_ameliaed = ldply(a.out$imputations) # combine all imputed datasets
train_data_ameliaed = train_data_ameliaed[c(-1,-12,-13)]  # delete .id and cabin, embarked

# impute test data
a.out2 = amelia(titanic_test_data, m=5, idvars=c(2, 3,4, 8, 10, 11),
                p2s=0)
titanic_test_ameliaed = ldply(a.out2$imputations)
titanic_test_ameliaed = titanic_test_ameliaed[c(-1,-11, -12)]  # delete .id and cabin, embarked

# train the model
svm.formula = as.formula(Survived~Pclass+Sex+Age+SibSp+Parch+Fare)
# tune the model
train_data_ameliaed_selected = select(train_data_ameliaed,
                                      Survived, Pclass,Sex, Age,
                                      SibSp,Parch,Fare)
tune.out = tune(svm, svm.formula, data = train_data_ameliaed,
                kernel="linear",
                ranges=list(cost=c(10, 100, 500, 1000)))
print(summary(tune.out))
svm.bestmod = tune.out$best.model
pred = predict(svm.bestmod)
true=train_data_ameliaed$Survived
temp_table = table(pred, true)
print("Correct classification ratio acheived:")
print(sum(diag(temp_table))/sum(temp_table))


# make prediction
titanic_test_ameliaed_selected = select(titanic_test_ameliaed,
                                        Pclass,Sex, Age,
                                        SibSp,Parch,Fare)
predict_svm_5sets = predict(svm.bestmod, newdata = titanic_test_ameliaed_selected)
# make majority vote
idx_temp = rep(seq(1,nrow(titanic_test_data)), 5)
titanic_pred_dt = data.table(idx=idx_temp, 
                             pred=as.numeric(levels(predict_svm_5sets)[predict_svm_5sets]))
titanic_pred = titanic_pred_dt[,lapply(.SD, sum), .SDcol="pred", by="idx"]
titanic_pred = titanic_pred$pred
titanic_pred[titanic_pred<=2]=0
titanic_pred[titanic_pred>2]=1 # majority vote
# write to file
titanic_test_data$Survived = titanic_pred
write.csv(select(titanic_test_data, PassengerId, Survived),
          file="prediction_amelia_svm.csv",
          row.names=FALSE)
