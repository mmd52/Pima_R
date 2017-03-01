#Author @ Mohammed 27/02/2017

#Loading Libraries
source("Libraries.R")

#Loading Data
source("Data.R")

data$Outcome<-as.factor(data$Outcome)
set.seed(999)
splitIndex<-createDataPartition(data$Outcome,p=.70,list=F,times=1)
train<-data[splitIndex,]
test<-data[-splitIndex,]
training_data<-train
testing_data<-test

#Running KNN Model

knn_pred<-knn(training_data[,-9],testing_data[,-9],training_data[,9],11)
caret::confusionMatrix(testing_data[,9], knn_pred, mode = "prec_recall")

#===========================AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(knn_pred))
print(auc)
plot(auc,print.auc=T)