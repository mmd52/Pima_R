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

#===================================================================
################################## Random Forest

# Tuning takes factors as target variables
bestmtry <- tuneRF(training_data[,-c(9)], as.factor(training_data[,9]), 
                   ntreeTry=1000, stepFactor=1.5, improve=0.01,
                   trace=TRUE, plot=TRUE, dobest=FALSE) 


rf.fit <- randomForest(Outcome ~ ., data=training_data, 
                       mtry=2, ntree=1000, keep.forest=TRUE, 
                       importance=TRUE,fold=10) 

varImpPlot(rf.fit)

# Confusion Matrix
preds <- predict(rf.fit, newdata=testing_data[,-9], type="response")
caret::confusionMatrix(testing_data[,9], preds, mode = "prec_recall")

#AUC
auc<-roc(as.numeric(testing_data[,9]),as.numeric(predict(rf.fit, newdata=testing_data[,-9], type="response")))
print(auc)
plot(auc,print.auc=T)