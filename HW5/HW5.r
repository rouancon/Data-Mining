#HW 5
#Connor Rouan and Yadukrishnan Sethumadhavan

#Problem 1
#Part A (pre-processing)
library(readxl)
toyota <- read_excel("ToyotaCorolla.xlsx", sheet = "data")
View(toyota)

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

#normalizing
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
toyota_n <- as.data.frame(lapply(toyota_d, normalize))

#splitting
train_toyota<- toyota_n[1:718,]
val_toyota<- toyota_n[719:1149,]
test_toyota<-toyota_n[1149:1436,]


#Part A, i
#build the model
library(rpart)
model <- rpart(price~Age_08_04+KM+Fuel_Type+HP+Automatic+Doors+Quarterly_Tax+Mfg_Guarantee+Guarantee_Period+Airco+Automatic_Airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, data=train_toyota, method=anova)
printcp(model)
#refine the model
opt_model <- prune(model, cp=)
#results of the model
rsq.rpart(opt_model)
print(opt_model)
plot(opt_model)
text(opt_model)


#Part A, ii
#predict the training data sets
pred <- predict(opt_model, newdata=train_toyota, type = c("vector"))
#Examine results
library(Metrics)
rmse(actual = train_toyota$price, predicted = pred)

#predict the validation data sets
pred <- predict(opt_model, newdata=val_toyota, type = c("vector"))
#Examine results
library(Metrics)
rmse(actual = val_toyota$price, predicted = pred)

#prediction data set
pred <- predict(opt_model, newdata=predict_toyota, type = c("vector"))
#Examine results
library(Metrics)
rmse(actual = predict_toyota$price, predicted = pred)


#Part A, iii
#predict the validation data sets on non-pruned model
pred <- predict(model, newdata=, type = c("vector"))
#Examine results
library(Metrics)
rmse(actual = val_toyota$price, predicted = pred)