#rm(list=ls())

setwd("d:/R/Churn")
library(readxl)
mydata<-read_excel("Churn.xls")
dim(mydata)
head(mydata)
class(mydata)

#convert tibble to dataframe
mydata<-as.data.frame(mydata)
str(mydata)
summary(mydata)

#Convert Phone column to numeric and Churn & State column as factor
mydata$Phone<-gsub("-", "", paste(mydata$Phone))
mydata$Phone=as.numeric(mydata$Phone)
mydata$State=as.factor(mydata$State)
mydata$Churn=as.factor(mydata$Churn)

#Checking the NA in the dataset and drawing missing Map; No missing data
library(Amelia)
sum(is.na(mydata))
any(is.na(mydata))
missmap(mydata)
# library(dplyr)
# library(purrr)
# mydata %>% map(~ sum(is.na(.)))


# Rename the columns of dataset
mydata<-setNames(mydata,c("Account.Length", "VMail.Message", "Day.Mins", "Eve.Mins", "Night.Mins", 
                          "Intl.Mins", "CustServ.Calls", "Churn", "Intl.Plan", "VMail.Plan", 
                          "Day.Calls", "Day.Charge", "Eve.Calls", "Eve.Charge", "Night.Calls", 
                          "Night.Charge", "Intl.Calls", "Intl.Charge", "State", "Area.Code", "Phone"))


# Rearrange the columns of the dataset
mydata<-mydata[,c(19,20,21,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,8)]

head(mydata)
str(mydata)
library(caret) 
set.seed(1234)
dataPart<-createDataPartition(mydata$Churn,p=.8,list=FALSE)
traindata<-mydata[dataPart,]
testdata<-mydata[-dataPart,]
dim(traindata);dim(testdata)
prop.table((table(traindata$Churn)))
prop.table((table(testdata$Churn)))


#Exploratory data analysis
#Find out the total number of churned customers in the train dataset
dev.off()
#plot.new()
text(barplot(table(traindata$Churn),col=c('#33FF33','#FF3333'),main='Bar plot of Churn'),0,
     table(traindata$Churn),cex=1.5,pos=3)


#rearrange the level of the factor variable 
#reorder_var<-reorder(traindata$Churn, new.order=c("1", "0"),sort=NULL)
reorder_var<- factor(traindata$Churn,levels = c("1","0"))


#Churn ratio by categorical predictors (red=churned)
library(ggplot2)
library(gridExtra)

plot.new()
exp1 <- ggplot(traindata, aes(x=as.factor(Area.Code), fill = reorder_var)) + geom_bar(position = "fill") + labs(x = "Area code", y = "") + theme(legend.position = "none")
exp2 <- ggplot(traindata, aes(x=as.factor(Intl.Plan), fill = reorder_var)) + geom_bar(position = "fill") + labs(x = "International", y = "") + theme(legend.position = "none")
exp3 <- ggplot(traindata, aes(x=as.factor(VMail.Plan), fill = reorder_var)) + geom_bar(position = "fill") + labs(x = "Voicemail", y = "") + theme(legend.position = "none")
exp4 <- ggplot(traindata, aes(x=as.factor(CustServ.Calls), fill = reorder_var)) + geom_bar(position = "fill") + labs(x = "Customer calls", y = "") + theme(legend.position = "none")
grid.arrange(exp1, exp2, exp3, exp4, ncol = 4, nrow = 1, top = "Churn/Non-churn Proportion")

#Explore distributions by continuous predictors
plot.new()
daymin <- ggplot(traindata, aes(reorder_var, Day.Mins, fill = reorder_var)) + xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
evemin <- ggplot(traindata, aes(reorder_var, Eve.Mins, fill = reorder_var)) + xlab("Churn") +  geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
nitmin <- ggplot(traindata, aes(reorder_var, Night.Mins, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
intmin <- ggplot(traindata, aes(reorder_var, Intl.Mins, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
daycal <- ggplot(traindata, aes(reorder_var, Day.Calls, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
evecal <- ggplot(traindata, aes(reorder_var, Eve.Calls, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
nitcal <- ggplot(traindata, aes(reorder_var, Night.Calls, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
intcal <- ggplot(traindata, aes(reorder_var, Intl.Calls, fill = reorder_var)) +  xlab("Churn") + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
grid.arrange(daymin, evemin, nitmin, intmin, 
             daycal, evecal, nitcal, intcal, 
             ncol = 4, nrow = 2)
plot.new()
exp6 <- ggplot(traindata, aes(Account.Length, fill = reorder_var)) + geom_density(alpha = 0.7) + theme(legend.position = "null")
exp7 <- ggplot(traindata, aes(VMail.Message, fill = reorder_var)) + geom_density(alpha = 0.7) + theme(legend.position = "null")
grid.arrange(exp6, exp7, ncol = 2, nrow = 1)

dev.off()
library(corrplot)
#Checking correlation in train dataset
corrplot(cor(traindata[sapply(traindata, is.numeric)]))

#Cross validation

library(caret) 
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)


############################
## Logistic Regression Model
############################

set.seed(1234)
log_model <- train(Churn~ + VMail.Message + Day.Mins + Eve.Mins + Night.Mins + Intl.Mins + 
                     CustServ.Calls + Intl.Plan + VMail.Plan + Day.Charge + Eve.Charge + 
                     Night.Charge + Intl.Calls + Intl.Charge, data = traindata, method = 'glm', 
                   metric = 'Accuracy', trControl = control, maxit=100)

summary(log_model)    

# AIC: 1723.8 improved with the addition of significant variable

#save(log_model, file = "D:/R/Churn/log_model.rda")

#Predictions & Accuracy for Train & Test data

#Prediction for Train data
Log_train<-predict(log_model,traindata)
head(Log_train)

#Accuracy for Train data
tab1<-table(Log_train,traindata$Churn)
Log_accuracytrain<-sum(diag(tab1))/sum(tab1) 
Log_accuracytrain         # 0.8728909

#Prediction for Test data
Log_test<-predict(log_model,testdata)
head(Log_test)

#Accuracy for Test data
tab2<-table(Log_test,testdata$Churn)
Log_accuracytest<-sum(diag(tab2))/sum(tab2) 
Log_accuracytest          #0.8543544



##########################################################
#Regression Model performance evaluation for Train dataset
##########################################################

library(ROCR)

pred<-predict(log_model, traindata, type='prob')
head(pred$`1`)
head(traindata$Churn)


pred1<-prediction(pred$`1`,traindata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.87,v=0.38)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]

#ROC (reciver operating characteristic) curve
pred1<-prediction(pred$`1`,traindata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="Sensitivity", xlab="1-Specficity")
abline(a=0,b=1)

#AUC (Area under curve)
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = 0.8)


#######################################################
#Logistic model performance evaluation for test dataset
#######################################################

logistic_predict<-predict(log_model, testdata, type='prob')
head(logistic_predict$`1`)
head(testdata$Churn)

pred1<-prediction(logistic_predict$`1`,testdata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.87,v=0.38)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]

#ROC (reciver operating characteristic) curve
pred1<-prediction(logistic_predict$`1`,testdata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="sensitivity", xlab="1-specficity")
abline(a=0,b=1)

#AUC (area under curve)
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = .8)



install.packages("Rserve")
library(Rserve)
Rserve(port = 6311)
