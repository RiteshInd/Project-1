#rm(list=ls())

setwd("d:/R")
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
#library(caret) 
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

#Logistic regression with all variables

set.seed(1234)
log_model_All <- train(Churn~ ., data = traindata, method = 'glm', 
                   metric = 'Accuracy', trControl = control, maxit=100)

summary(log_model_All)    

# AIC: 1738.8



#Feature selection approach using random forest method
library(party)
set.seed(1234)
#options(scipen=999)
cf1 <- cforest(Churn ~ . , data= mydata, control=cforest_unbiased(mtry=4,ntree=50))
varimp(cf1)

# State           Area.Code       Phone           Account.Length  VMail.Message   Day.Mins        Eve.Mins 
# -0.00006525285  -0.00035889070  -0.00042414356  0.00060358891   0.00946166395   0.03588907015   0.01086460033 
# Night.Mins      Intl.Mins       CustServ.Calls  Intl.Plan       VMail.Plan      Day.Calls       Day.Charge 
# 0.00123980424   0.00691680261   0.03368678630   0.02942903752   0.00533442088   0.00063621533   0.02938009788 
# Eve.Calls       Eve.Charge      Night.Calls     Night.Charge    Intl.Calls      Intl.Charge 
# -0.00006525285  0.01004893964   0.00027732463   0.00114192496   0.01063621533   0.00497553018 


#Feature selection using Boruta method
library(Boruta)
set.seed(1234)
# perform Boruta search
boruta_output <- Boruta(Churn ~ ., data=na.omit(mydata), doTrace=2)  
# collect Confirmed and Tentative variables
boruta_signif <- names(boruta_output$finalDecision
                       [boruta_output$finalDecision %in% c("Confirmed", "Tentative")]) 
# significant variables
print(boruta_signif)  

# [1] "VMail.Message"  "Day.Mins"     "Eve.Mins"     "Night.Mins"   "Intl.Mins"      "CustServ.Calls"
# [7] "Intl.Plan"      "VMail.Plan"   "Day.Charge"   "Eve.Charge"   "Night.Charge"   "Intl.Calls"    
# [13] "Intl.Charge"


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

# Summary for Logistic Regression model
# Accuracy for train data : 0.8728909; Accuracy for test data : 0.8543544.
# Performance of model for train data : 0.8740157; Optimal Threshold : 0.4733843; AUC : 0.8554
# Performance of model for test data : 0.8648649; Optimal Threshold : 0.657697; AUC : 0.7537


################
# Boosted Tree
################
library(C50)
set.seed(1234)
C5_model <- train(Churn ~ + VMail.Message + Day.Mins + Eve.Mins + Night.Mins + Intl.Mins + 
                    CustServ.Calls + Intl.Plan + VMail.Plan + Day.Charge + Eve.Charge + 
                    Night.Charge + Intl.Calls + Intl.Charge, data = traindata, 
                  method = "C5.0", metric = 'Accuracy',  trControl = control, maxit=100)

print(C5_model)
plot(C5_model)

#Predictions & accuracy for train data
gb_predtr <- predict(C5_model, newdata = traindata)

confusionMatrix<- confusionMatrix(gb_predtr,traindata$Churn)
confusionMatrix

#Predictions & accuracy for test data

gb_pred <- predict(C5_model, newdata = testdata)

tab2<-table(gb_pred,testdata$Churn)
acc_test<-sum(diag(tab2))/sum(tab2)

#########################################################
#Boosted Tree Model performance evaluation for train data
#########################################################
library(ROCR)

pred<-predict(C5_model, traindata, type='prob')
head(pred$`1`)
head(traindata$Churn)

pred1<-prediction(pred$`1`,traindata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.99,v=0.4)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]

#ROC curve
pred1<-prediction(pred$`1`,traindata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="sensitivity", xlab="1-specficity")
abline(a=0,b=1)

#AUC 
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = .8)

#########################################################
#Boosted Tree Model performance evaluation for Test data
#########################################################

C5_predict<-predict(C5_model ,testdata,type='prob')
head(C5_predict$`1`)
head(testdata$Churn)

pred1<-prediction(C5_predict$`1`,testdata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.97,v=0.55)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]

#ROC curve
pred1<-prediction(C5_predict$`1`,testdata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="sensitivity", xlab="1-specficity")
abline(a=0,b=1)

#AUC 
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)       
legend(.8,.2,auc,title = "AUC",cex = .8)

# Summary for Boosted Tree model
# Accuracy for Train data: Accuracy : 0.982; Sensitivity : 1.0000; Specificity : 0.8760
# Accuracy for Test data: 0.9444444
# Performance of model for train data : 0.9835021; Optimal Threshold: 0.4369573; AUC : 0.9977
# Performance of model for test data : 0.9474474; Optimal Threshold : 0.5621201; AUC : 0.9007



################
## Random forest
################

library(randomForest)

set.seed(1234)
mtry <- sqrt(ncol(mydata))
rf_random<- train(Churn~ + VMail.Message + Day.Mins + Eve.Mins + Night.Mins + Intl.Mins + 
                    CustServ.Calls + Intl.Plan + VMail.Plan + Day.Charge + Eve.Charge + 
                    Night.Charge + Intl.Calls + Intl.Charge, data = traindata, method = 'rf', 
                  metric = 'Accuracy',  trControl = control, maxit=100)
print(rf_random)
plot(rf_random)

#predictions for Train data
predictions<- predict(rf_random,traindata)

#Accuracy for Train data
tab1<-table(predictions,traindata$Churn)
accuracy<-sum(diag(tab1))/sum(tab1)
#or
confusionMatrix<- confusionMatrix(predictions,traindata$Churn)
confusionMatrix

#Prediction for test data
predictions1<- predict(rf_random,testdata)

#Accuracy for test data
tab2<-table(predictions1,testdata$Churn)
accuracy2<-sum(diag(tab2))/sum(tab2)

##########################################################
#Random Forest Model performance evaluation for Train data
##########################################################

library(ROCR)

pred<-predict(rf_random ,traindata,type='prob')
head(pred$`1`)
head(traindata$Churn)

pred1<-prediction(pred$`1`,traindata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=1,v=0.4)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]

#ROC curve
pred1<-prediction(pred$`1`,traindata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="sensitivity", xlab="1-specficity")
abline(a=0,b=1)

#AUC 
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = .8)

#########################################################
#Random Forest Model performance evaluation for Test data
#########################################################

rf_predict<-predict(rf_random ,testdata,type='prob')
head(rf_predict$`1`)
head(testdata$Churn)

pred1<-prediction(rf_predict$`1`,testdata$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.94,v=0.55)

#identify bestvalues
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cutoff<-slot(eval,"x.values")[[1]][max]

#ROC curve
pred1<-prediction(rf_predict$`1`,testdata$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#Adding colors, x and y labels
plot(roc, colorize=T, main="ROC curve", ylab="sensitivity", xlab="1-specficity")
abline(a=0,b=1)

#AUC 
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = .8)

# Summary for Random Forest model
# Accuracy for train data : 1; Sensitivity : 1.0000; Specificity : 1.0000
# Accuracy for test data : 0.9414414
# Performance for train data accuracy : 1; Optimal Threshold : 0.598; AUC : 1
# Performance for test data accuracy : 0.9459459; Optimal Threshold : 0.398; AUC : 0.8966

install.packages("Rserve")
library(Rserve)
Rserve(port = 6311)
