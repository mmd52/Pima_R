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

#Running Naive Bayes Model

model<-naiveBayes(Outcome~.,data = training_data)

preds<-predict(model,testing_data[,-9])

caret::confusionMatrix(testing_data[,9], preds, mode = "prec_recall")

#===========================AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(predict(model,testing_data[,-9])))
print(auc)
plot(auc,print.auc=T)