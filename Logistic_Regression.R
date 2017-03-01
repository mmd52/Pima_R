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

#Running Logistic Regression

logit.fit=glm(Outcome~.,family=binomial(logit),data=training_data)

summary(logit.fit)
vif(logit.fit)
#Great No multicolinearity here

preds<-ifelse(predict(logit.fit,newdata=testing_data[,-9],type="response")>=0.42,1,0)
caret::confusionMatrix(testing_data[,9], preds, mode = "prec_recall")

#=================AUC
auc<-roc(testing_data[,9],ifelse(predict(logit.fit,newdata=testing_data[,-9],type="response")>=0.42,1,0))
print(auc)
plot(auc,print.auc=T)