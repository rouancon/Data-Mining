#HW 6
#Connor Rouan and Yadukrishnan Sethumadhavan

#--Problem 1--
#Part A

library(readxl)
toyota <- read_excel("Documents/GitHub/DataMining-HW1/Data-Mining/HW6/ToyotaCorolla.xlsx")

toyota<- toyota[c("Price","Age_08_04","KM","Fuel_Type","HP","Automatic","Doors",
                  "Quarterly_Tax","Mfr_Guarantee","Guarantee_Period","Airco",
                  "Automatic_airco","CD_Player","Powered_Windows","Sport_Model",
                  "Tow_Bar")]

#converting Fule_Type to categorical variable
toyota$Fuel_Type <- factor(toyota$Fuel_Type)
toyota$Fuel_Type <- as.numeric(toyota$Fuel_Type)
toyota$Fuel_Type <- factor(toyota$Fuel_Type)



#converting to dummy variable
library(caret)
dmy <- dummyVars("~.", data = toyota,fullRank = F)
toyota_d <- data.frame(predict(dmy, newdata = toyota))

#normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
toyota_n <- as.data.frame(lapply(toyota_d, normalize))
View(toyota_n)

#splitting
train_toyota<- toyota_n[1:1077,]
val_toyota<- toyota_n[1077:1436,]

#Build NN
library(neuralnet)
model <- neuralnet(
  formula = Price~Age_08_04+KM+Fuel_Type.1+Fuel_Type.2+Fuel_Type.3+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, 
  train_toyota,
  hidden = 1,
  threshold = .001,
  stepmax = 1e+05, 
  rep = 1, 
  startweights = NULL,
  learningrate.limit = NULL,
  learningrate.factor = list(minus = 0.5, plus = 1.2),
  learningrate=NULL,
  lifesign = "minimal",
  lifesign.step = 1000,
  algorithm = "rprop+",
  err.fct = "sse",
  act.fct = "logistic",
  linear.output = TRUE,
  exclude = NULL,
  constant.weights = NULL,
  likelihood = FALSE)

#Part A
#for threshold values, 1, 0.1, 0.05, 0.01, 0.005, 0.001, and 0.0001 on training data
#prediction
pred_toyota <- train_toyota
pred_toyota$Price <- NULL
result <- compute(model, pred_toyota)

#error evaluation
library(ModelMetrics)
rmse(train_toyota$Price, result$net.result)
data.frame(actual = train_toyota$Price, prediction = result$net.result)

#Part B
#for threshold values, 1, 0.1, 0.05, 0.01, 0.005, 0.001, and 0.0001 on prediction data
#prediction
pred_toyota <- val_toyota
pred_toyota$Price <- NULL
result <- compute(model, pred_toyota)

#error evaluation
library(ModelMetrics)
<<<<<<< HEAD
rmse(train_toyota$Price, result$net.result)
data.frame(actual = train_toyota$Price, prediction = result$net.result)
=======
rmse(train_toyota$Price, result)
rmse
>>>>>>> ef183bf3b74fd03b41f68bf977f722175c31d976
