##importing data

getwd()
setwd("C:/Users/kriti/Documents/mckinseyhackathon/train_5ZizJYZ")
getwd()
train<-read.csv("train.csv", na.strings=c("", "NA"))

##data treatment

##identifying missing values
summary(train)
missing_data<-apply(is.na(train),2,sum)
missing_data<-data.frame(missing_data)

##treating missing values


#imputing the following columns
#Loan_Amount                         Loan_Period 
#Interest_Rate                                 EMI 
#eisting_EMi

##sdx<-function(x){
#t=(mean(x)/sd(x)) 
# return(t)}
#sdx(train$Loan_Amount1)

##replacing loan_amount
train$Loan_Amount[is.na(train$Loan_Amount)]<-0
mean1_LA<-mean(train$Loan_Amount)
train$Loan_Amount[train$Loan_Amount==0]<-mean1_LA

##replacing Loan_period

train$Loan_Period[is.na(train$Loan_Period)]<-0
mean1_LP<-mean(train$Loan_Period)
train$Loan_Period[train$Loan_Period==0]<-mean1_LP

##replacing Interest Rate
train$Interest_Rate[is.na(train$Interest_Rate)]<-0
mean1_IR<-mean(train$Interest_Rate)
train$Interest_Rate[train$Interest_Rate==0]<-mean1_IR


##replacing EMI
train$EMI[is.na(train$EMI)]<-0
mean1_EM<-mean(train$EMI)
train$EMI[train$EMI==0]<-mean1_EM

##replacing existing EMI
train$Existing_EMI[is.na(train$Existing_EMI)]<-0
mean1_EM1<-mean(train$Existing_EMI)
train$Existing_EMI[train$Existing_EMI==0]<-mean1_EM1

##qc
missing_data<-apply(is.na(train),2,sum)
missing_data<-data.frame(missing_data)

##eliminating all the blank char values
##13% data eliminated
train2<-na.omit(train)

##qc
missing_data<-apply(is.na(train2),2,sum)
missing_data<-data.frame(missing_data)


##removing IDs
train_f<-train2[,-c(1,7,14)]



#checking outliers

train_org<-train2
train2<-train_f

#########################monthly income


plot(train2$Monthly_Income)
max<-max(train2$Monthly_Income)

##one max value removed
train2<-subset(train2,train2$Monthly_Income!=max)
plot(train2$Monthly_Income)
boxplot(train2$Monthly_Income)
summary(train2$Monthly_Income)

##checking quantile values
quantile(train2$Monthly_Income,c(0.90,0.999))


201440.4

##capping value at 99.9%

train2$Monthly_Income<-ifelse(train2$Monthly_Income>112280,112280,train2$Monthly_Income)
plot(train2$Monthly_Income)


##after outliers are removed we are normalizing the variable
##since income has high values we are normalizing the variables

train2$income_zscore<-(train2$Monthly_Income-mean(train2$Monthly_Income))/sd(train2$Monthly_Income)

#####################################Existing EMI
plot(train2$EMI)
quantile(train2$EMI)

##do not see outliers

##just comouting the normalized scores

train2$emi_zscore<-(train2$EMI-mean(train2$EMI))/sd(train2$EMI)
plot(train2$emi_zscore)


##################################Loan Amount

plot(train2$Loan_Amount)
quantile(train2$Loan_Amount)
quantile(train2$Loan_Amount,c(0.90,0.99,0.9999))

##capping at 99.99 percentile

ifelse(train2$Loan_Amount>250319.2,250319.2,train2$Loan_Amount)

##calculating z-scores

train2$Loan_Amount_zcore<-(train2$Loan_Amount-mean(train2$Loan_Amount))/sd(train2$Loan_Period)
plot(train2$Loan_Amount_zcore)


##############################Existing EMI

plot(train2$Existing_EMI)
quantile(train2$Existing_EMI,c(0.90,0.99,0.9999))

sum(ifelse(train2$Existing_EMI>20114,1,0))

##capping values at 9.99 percentile
train2$Existing_EMI<-ifelse(train2$Existing_EMI>20114,20114,train2$Existing_EMI)
plot(train2$Existing_EMI)
boxplot(train2$Existing_EMI)

##calculating z-variables

train2$Existing_EMI_zcores<-(train2$Existing_EMI-mean(train2$Existing_EMI))/sd(train2$Existing_EMI)
plot(train2$Existing_EMI_zcores)


###############################

###creating training and test datasets


require(caTools)
sample = sample.split(train2$Approved, SplitRatio = .70)
train_f = subset(train2, sample == TRUE)
test_f  = subset(train2, sample == FALSE)


##we try binning cities

##removing C part from the city codes

train_f$City_Code_m<-substr(train_f$City_Code,2,length(train_f$City_Code))
test_f$City_code_m<-substr(test_f$City_Code,2,length(test_f$City_Code))

##mapping states

##adding on train data
library(zipcode)
data(zipcode)
train_f<-merge(train_f,zipcode,by.x="City_Code_m",by.y="zip",all.x=TRUE)
train_f<-data.frame(train_f)
train_f$state<-as.factor(train_f$state)
train_f$city<-as.factor(train_f$city)

##adding on test data

test_f<-merge(test_f,zipcode,by.x="City_code_m",by.y="zip",all.x=TRUE)
test_f<-data.frame(test_f)
test_f$state<-as.factor(test_f$state)
test_f$city<-as.factor(test_f$city)

##extracting month lead creation date

train_f$Lead_creation_month<-as.factor(format(as.Date(train_f$Lead_Creation_Date),"%m"))
test_f$Lead_creation_month<-as.factor(format(as.Date(test_f$Lead_Creation_Date),"%m"))


#getting the age of each customer

test_f$age<-as.numeric(format(Sys.Date(),"%d"))-as.numeric(substr(test_f$DOB,1,2))

##checking correlations between variables

##combining all numeric varaibles

train_f_n<-train_f[,c(9,14,15,16,17,18)]

(cor(train_f_n))

pairs(~.,data=train_f_n)

library(corrplot)

corrplot(cor(train_f_n),type=scatter)

plot(train_f$EMI,train_f$Loan_Amount)

##emi and loan amount positively correlated


##trying gbm
library(gbm)

##squares of variables with high rel influence



train_f$income2<-train_f$income_zscore*train_f$income_zscore
test_f$income2<-test_f$income_zscore*test_f$income_zscore



train_f$Interest_Rate2<-train_f$Interest_Rate*train_f$Interest_Rate
test_f$Interest_Rate2 <-test_f$Interest_Rate*test_f$Interest_Rate

train_f$Existing_EMI_zcores2<-train_f$Existing_EMI_zcores*train_f$Existing_EMI_zcores
test_f$Existing_EMI_zcores2<-test_f$Existing_EMI_zcores*test_f$Existing_EMI_zcores



#time since lead creating
train_f$time<-as.numeric(format(Sys.Date(),"%d"))-as.numeric(format(as.Date(train_f$Lead_Creation_Date),"%y"))
test_f$time<-as.numeric(format(Sys.Date(),"%d"))-as.numeric(format(as.Date(test_f$Lead_Creation_Date),"%y"))

train_f$log_time<-log(abs(train_f$time))
test_f$log_time<-log(abs(test_f$time))




tree_gbm1=gbm(Approved~Interest_Rate+Var1+
                income_zscore+Primary_Bank_Type+Loan_Period+emi_zscore+
                Loan_Period+state+Existing_EMI_zcores+
                time+income2+Existing_EMI_zcores2+Source_Category
              ,data=train_f,distribution="gaussian",n.trees = 500,
              interaction.depth=10)

summary(tree_gbm1)

##predicting on test data
#2,8,15 are id columns

tree_gbm_pred1=predict(tree_gbm1,newdata=test_f,n.trees = 500)

library(pROC)
auc_t1 <- roc(test_f$Approved,tree_gbm_pred1)

auc_t1


###we now work on fine tuning the model
library(gbm)
library(caret)
library(Metrics)
library(mlbench)
library(e1071)
fitcontrol<-trainControl(method="repeatedcv",number=4,repeats=4)

#refer this link-
#https://www.analyticsvidhya.com/blog/2016/02/complete-guide-parameter-tuning-gradient-boosting-gbm-python/




tree_gbm1=gbm(Approved~Interest_Rate+Var1+
                income_zscore+Primary_Bank_Type+Loan_Period+emi_zscore+
                Loan_Period+state+Existing_EMI_zcores+
                time+income2+Existing_EMI_zcores2+Source_Category
              ,data=train_f,distribution="gaussian",n.trees = 500,
              interaction.depth=10,n.minobsinnode=5000,shrinkage=0.1,bag.fraction=0.85,
              verbose=FALSE)

summary(tree_gbm1)

#tree_gbm1=gbm(Approved~Interest_Rate+Var1+
                #income_zscore+Primary_Bank_Type+Loan_Period+emi_zscore+
               # Loan_Period+state+Existing_EMI_zcores+
               # time+income2+Existing_EMI_zcores2+Source_Category
              #,data=train_f,distribution="gaussian",n.trees = 500,
             # interaction.depth=10,n.minobsinnode=5000,shrinkage=0.1,bag.fraction=0.85,
             # verbose=FALSE)

#summary(tree_gbm1)

##predicting on test data
#2,8,15 are id columns

tree_gbm_pred1=predict(tree_gbm1,newdata=test_f,n.trees = 500)

library(pROC)
auc_t1 <- roc(test_f$Approved,tree_gbm_pred1)

auc_t1

##the finetuning results in a decrease in the model performance


library(caret)
install.packages("psych")

##finetunig using caret package finally

library(Metrics)

##creating a grid of tuning parameters

caret_grid<-expand.grid(interaction.depth=c(1,3,5),n.trees=500,shrinkage=c(0.01,0.001)
                        )

metric<-"Accuracy"
trainControl <- trainControl(method="cv", number=10)



gbm.caret <- train(Approved~Interest_Rate+Var1+
                     income_zscore+Primary_Bank_Type+Loan_Period+emi_zscore+
                     Loan_Period+Existing_EMI_zcores+
                     time+income2+Existing_EMI_zcores2+Source_Category
                   ,data=train_f,distribution="gaussian", method="gbm",
                   trControl=trainControl, verbose=FALSE, 
                   tuneGrid=caretGrid, bag.fraction=0.75)  


print(caret_grid)
