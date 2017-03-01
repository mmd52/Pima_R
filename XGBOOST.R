#Author @ Mohammed 27/02/2017

#Loading Libraries
source("Libraries.R")

#Loading Data
source("Data.R")

#Splitting data into Train and Test
set.seed(999)
splitIndex<-createDataPartition(data$Outcome,p=.70,list=F,times=1)
train<-data[splitIndex,]
test<-data[-splitIndex,]
training_data<-train
testing_data<-test

#====================================================================
######################## Preparing for xgboost

dtrain = xgb.DMatrix(as.matrix(training_data[,-9]), 
                     label=training_data[,9])
dtest = xgb.DMatrix(as.matrix(testing_data[,-9]))

#XGBOOST
xgb_param_adult = list(
  nrounds = c(700),
  eta = 0.02,#eta between(0.01-0.2)
  max_depth = 5, #values between(3-10)
  subsample = 0.7,#values between(0.5-1)
  colsample_bytree = 0.7,#values between(0.5-1)
  num_parallel_tree=1,
  objective='binary:logistic',
  min_child_weight = 1,
  booster='gbtree'
)

res = xgb.cv(xgb_param_adult,
             dtrain,
             nrounds=700,   # changed
             nfold=10,           # changed
             early_stopping_rounds=50,
             print_every_n = 10,
             verbose= 1)

xgb.fit = xgb.train(xgb_param_adult, dtrain, 500)


# Confusion Matrix
preds <- ifelse(predict(xgb.fit, newdata=as.matrix(testing_data[,-9])) >=0.59, 1, 0)
caret::confusionMatrix(testing_data[,9], preds, mode = "prec_recall")

#===========================AUC
auc<-roc(testing_data[,9],predict(xgb.fit,dtest))
print(auc)
plot(auc,print.auc=T)