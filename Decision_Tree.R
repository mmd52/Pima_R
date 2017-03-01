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

#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(Outcome~.,data=training_data,control=Weka_control(),options=NULL)

summary(j48)

preds<-predict(j48,testing_data[,-9])
caret::confusionMatrix(testing_data[,9], preds, mode = "prec_recall")

#===========================AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(predict(j48,testing_data[,-9])))
print(auc)
plot(auc,print.auc=T)