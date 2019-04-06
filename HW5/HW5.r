#HW 5
#Connor Rouan and Yadukrishnan Sethumadhavan

#-- Problem 1 --
#Part A (pre-processing)
library(readxl)
toyota <- read_excel("ToyotaCorolla.xlsx", sheet = "data")

#converting Fule_Type to categorical variable
toyota$Fuel_Type <- factor(toyota$Fuel_Type)
toyota$Fuel_Type <- as.numeric(toyota$Fuel_Type)
toyota$Fuel_Type <- factor(toyota$Fuel_Type)

#converting Color to categorical variable
toyota$Color <- factor(toyota$Color)
toyota$Color <- as.numeric(toyota$Color)
toyota$Color <- factor(toyota$Color)

toyota<- toyota[c("Price","Age_08_04","KM","Fuel_Type","HP","Automatic","Doors","Quarterly_Tax","Mfr_Guarantee",
                  "Guarantee_Period","Airco","Automatic_airco","CD_Player","Powered_Windows","Sport_Model",
                  "Tow_Bar")]

#converting to dummy variable
library(caret)
dmy <- dummyVars("~.", data = toyota,fullRank = F)
toyota_d <- data.frame(predict(dmy, newdata = toyota))

#splitting
train_toyota<- toyota_d[1:718,]
val_toyota<- toyota_d[719:1149,]
test_toyota<-toyota_d[1149:1436,]


#Part A, i
#build the model
library(rpart)
library(rpart.plot)
rt_model <- rpart(Price~Age_08_04+KM+Fuel_Type.1+Fuel_Type.2+Fuel_Type.3+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, data=train_toyota, control = rpart.control(maxdepth = 8), method="anova")
printcp(rt_model)
#refine the model
opt_rt_model <- prune(rt_model, cp=.01)
#results of the model
rsq.rpart(opt_rt_model)
rpart.plot(opt_rt_model)
text(opt_rt_model)


#Part A, ii
#predict the TRAINING data sets
train_predict <- train_toyota
train_predict$Price <- NULL
pred <- predict(object = opt_rt_model, newdata=train_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(train_toyota$Price, pred)
pred <- as.data.frame(predict(object = opt_rt_model, newdata=train_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = train_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Training Error", xlab="Predicted Price", ylab="Actual Price")

#predict the VALIDATION data sets
val_predict <- val_toyota
val_predict$Price <- NULL
pred <- predict(object = opt_rt_model, newdata=val_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(val_toyota$Price, pred)
pred <- as.data.frame(predict(object = opt_rt_model, newdata=val_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = val_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Validation Error", xlab="Predicted Price", ylab="Actual Price")

#predict TEST data set
test_predict <- test_toyota
test_predict$Price <- NULL
pred <- predict(opt_rt_model, newdata=test_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(test_toyota$Price, pred)
pred <- as.data.frame(predict(opt_rt_model, newdata=test_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = test_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Test Prediction Error", xlab="Predicted Price", ylab="Actual Price")


#Part A, iii
#predict the TRAINING data sets with NON-pruned model
train_predict <- train_toyota
train_predict$Price <- NULL
pred <- predict(object = rt_model, newdata=train_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(train_toyota$Price, pred)
pred <- as.data.frame(predict(object = rt_model, newdata=train_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = train_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Non-Pruned Training Error", xlab="Predicted Price", ylab="Actual Price")

#Part B, i

library(readxl)
toyota <- read_excel("ToyotaCorolla.xlsx", sheet = "data")


toyota<- toyota[c("Price","Age_08_04","KM","Fuel_Type","HP","Automatic","Doors","Quarterly_Tax","Mfr_Guarantee",
                  "Guarantee_Period","Airco","Automatic_airco","CD_Player","Powered_Windows","Sport_Model",
                  "Tow_Bar")]

#converting Fule_Type to categorical variable
toyota$Fuel_Type <- factor(toyota$Fuel_Type)
toyota$Fuel_Type <- as.numeric(toyota$Fuel_Type)
toyota$Fuel_Type <- factor(toyota$Fuel_Type)

#converting to dummy variable
library(caret)
dmy <- dummyVars("~.", data = toyota,fullRank = F)
toyota_d <- data.frame(predict(dmy, newdata = toyota))

#splitting price into 20 bins of equal counts
toyota_d<-cbind(new_price=0, toyota_d)
toyota_d$new_price<-cut(toyota_d$Price, 20, labels=FALSE)
#View(toyota)

toyota_d$Price <- NULL

train_toyota<- toyota_d[1:718,]
val_toyota<- toyota_d[719:1149,]
test_toyota<-toyota_d[1149:1436,]
#View(train_toyota)

ct_model <- rpart(new_price~Age_08_04+KM+Fuel_Type.1+Fuel_Type.2+Fuel_Type.3+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, data=train_toyota, control = rpart.control(maxdepth = 8), method="class")

rpart.plot(ct_model)


#Part B, ii

pred_data<-data.frame("Age_08_04"= 77,"KM"=117000, "Fuel_Type.1"=0,"Fuel_Type.2"=0,"Fuel_Type.3"=1 ,
                      "HP"=110 ,"Automatic"= 0 ,"Doors"= 5,"Quarterly_Tax"= 100 ,"Mfr_Guarantee"=0 ,
                      "Guarantee_Period"= 3,"Airco"= 1,"Automatic_airco"= 0,"CD_Player"=0 ,
                      "Powered_Windows"= 0,"Sport_Model"=0 ,"Tow_Bar"=1)

rt_pred <- predict(object = opt_rt_model, newdata=pred_data, type = "vector")
ct_pred <- predict(object = ct_model, newdata=pred_data, type = "prob")


#-- Problem 2 --
banks <- read_excel("Banks.xlsx")
View(banks)
str(banks)
banks$"Financial Condition"<-factor(banks$"Financial Condition")
X1<-banks$"Financial Condition"
X2<-banks$`TotExp.Assets`
X3<-banks$`TotLns.Lses.Assets`

#logit as a function
fit.full <- glm(X1 ~ X2+X3,data=banks,family=binomial())
summary(fit.full)

#reduced fit model
fit.reduced <- glm(X1 ~ X2,data=banks,family=binomial())
summary(fit.full)

#compare the fit
anova(fit.reduced, fit.full, test="Chisq")

#Part unknown
coef(fit.full)
exp(coef(fit.full))

testData <- data.frame(X2,X3)
testData$prob <- predict(fit.full, newdata=testData, type="response")
testData$real <- X1
testData

#Part B
new_bank<-data.frame("X2"=0.11, "X3"=0.6)

new_bank$prob <- predict(fit.full, newdata=new_bank, type="response")
new_bank
coef(fit.full)
exp(coef(fit.full))

#-- Problem 3 --
#Part A


#Part B
admins <- read_excel("sys_adminis.xlsx")
admins$`Completed task` <- ifelse(admins$`Completed task`=="Yes", 1, 0)

#logit as a function
fit.full <- glm(`Completed task` ~ Experience+Training, data=admins, family=binomial())
summary(fit.full)

#use the model to predict
testData <- data.frame("Experience" = admins$Experience, "Training" = admins$Training)
testData$pred <- predict(fit.full, newdata=testData, type="response")
testData$act <- admins$`Completed task`
#intrepret classification probabilities
testData$pred <- ifelse(testData$pred > 0.5, 1, 0)
count = 0
total = 0
for (row in 1:nrow(testData)) {
  pred <- testData[row, "pred"]
  act  <- testData[row, "act"]
  
  if(pred == 0 && act == 1) {count=count+1}
  if(act == 1) {total=total+1}
}
percentage = count/total
percentage

#Part C
admins <- read_excel("sys_adminis.xlsx")
admins$`Completed task` <- ifelse(admins$`Completed task`=="Yes", 1, 0)

#logit as a function
fit.full <- glm(`Completed task` ~ Experience+Training, data=admins, family=binomial())
summary(fit.full)

#use the model to predict
testData <- data.frame("Experience" = admins$Experience, "Training" = admins$Training)
testData$pred <- predict(fit.full, newdata=testData, type="response")
testData$act <- admins$`Completed task`
#intrepret classification probabilities
testData$pred <- ifelse(testData$pred > 0.3, 1, 0)
count = 0
total = 0
for (row in 1:nrow(testData)) {
  pred <- testData[row, "pred"]
  act  <- testData[row, "act"]
  
  if(pred == 0 && act == 1) {count=count+1}
  if(act == 1) {total=total+1}
}
percentage = count/total
percentage

#Part D
admins <- read_excel("sys_adminis.xlsx")
admins$`Completed task` <- ifelse(admins$`Completed task`=="Yes", 1, 0)

#logit as a function
fit.full <- glm(`Completed task` ~ Experience+Training, data=admins, family=binomial())
summary(fit.full)

#use the model to predict
testData <- data.frame("Experience" = admins$Experience, "Training" = admins$Training)
testData$pred <- predict(fit.full, newdata=testData, type="response")
testData$act <- admins$`Completed task`
#intrepret classification probabilities
testData$pred <- ifelse(testData$pred > 0.5, 1, 0)
min_exp = 100000000
count = 0
total_exp = 0
for (row in 1:nrow(testData)) {
  train <- testData[row, "Training"]
  exp <- testData[row, "Experience"]
  pred  <- testData[row, "pred"]
  
  if(pred == 1 && train == 4) {count=count+1
                                total_exp=total_exp+exp}
  if(pred == 1 && train == 4 && exp < min_exp) {min_exp=exp}
}
avg <- total_exp/count
avg
min_exp
