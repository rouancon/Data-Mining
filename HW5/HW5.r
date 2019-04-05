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
model <- rpart(Price~Age_08_04+KM+Fuel_Type.1+Fuel_Type.2+Fuel_Type.3+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, data=train_toyota, control = rpart.control(maxdepth = 8), method="anova")
printcp(model)
#refine the model
opt_model <- prune(model, cp=.01)
#results of the model
rsq.rpart(opt_model)
rpart.plot(opt_model)
text(opt_model)


#Part A, ii
#predict the TRAINING data sets
train_predict <- train_toyota
train_predict$Price <- NULL
pred <- predict(object = opt_model, newdata=train_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(train_toyota$Price, pred)
pred <- as.data.frame(predict(object = opt_model, newdata=train_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = train_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Training Error", xlab="Predicted Price", ylab="Actual Price")

#predict the VALIDATION data sets
val_predict <- val_toyota
val_predict$Price <- NULL
pred <- predict(object = opt_model, newdata=val_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(val_toyota$Price, pred)
pred <- as.data.frame(predict(object = opt_model, newdata=val_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = val_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Validation Error", xlab="Predicted Price", ylab="Actual Price")

#predict TEST data set
test_predict <- test_toyota
test_predict$Price <- NULL
pred <- predict(opt_model, newdata=test_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(test_toyota$Price, pred)
pred <- as.data.frame(predict(opt_model, newdata=test_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = test_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Test Prediction Error", xlab="Predicted Price", ylab="Actual Price")


#Part A, iii
#predict the TRAINING data sets with NON-pruned model
train_predict <- train_toyota
train_predict$Price <- NULL
pred <- predict(object = model, newdata=train_predict, type = "vector")
#Examine results
library(ModelMetrics)
rmse(train_toyota$Price, pred)
pred <- as.data.frame(predict(object = model, newdata=train_predict, type = "vector"))
names(pred)[1] <- "predPrice"
boxData<-data.frame("pred" = pred, "act" = train_toyota$Price)
boxplot(act~unlist(pred),data=boxData, main="Non-Pruned Training Error", xlab="Predicted Price", ylab="Actual Price")


#-- Problem 2 --
