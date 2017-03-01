#Author @ Mohammed 27/02/2017

#Loading Libraries
source("Libraries.R")

#Loading Data
source("Data.R")

#Splitting data into Train and Test
data$Outcome<-as.factor(data$Outcome)
set.seed(999)
splitIndex<-createDataPartition(data$Outcome,p=.70,list=F,times=1)
train<-data[splitIndex,]
test<-data[-splitIndex,]
training_data<-train
testing_data<-test

#=============== Running simple SVM
svm_simple <- svm(Outcome ~ ., data = training_data)
preds_simple = predict(svm_simple,testing_data[,-9])
caret::confusionMatrix(testing_data[,9], preds_simple, mode = "prec_recall")
#AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(preds_simple))
print(auc)
plot(auc,print.auc=T)

#============== SVM Complex with folds
tuneResult <- tune(svm, Outcome~.,  data = training_data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9),folds=10))

print(tuneResult)
tunedModel <- tuneResult$best.model
preds_complex1 <- predict(tunedModel, testing_data[,-9]) 
caret::confusionMatrix(testing_data[,9], preds_complex1, mode = "prec_recall")

#AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(preds_complex1))
print(auc)
plot(auc,print.auc=T)

#============== SVM Complex with folds 2
tuneResult <- tune(svm, Outcome~.,  data = training_data,
                   ranges = list(epsilon = seq(0,1,0.1), cost =(2:9),folds=10,
                                kernel="linear"))

print(tuneResult)
tunedModel <- tuneResult$best.model
preds_complex2 <- predict(tunedModel, testing_data[,-9]) 
caret::confusionMatrix(testing_data[,9], preds_complex2, mode = "prec_recall")

#AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(preds_complex2))
print(auc)
plot(auc,print.auc=T)