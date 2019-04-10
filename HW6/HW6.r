#HW 6
#Connor Rouan and Yadukrishnan Sethumadhavan

#--Problem 1--
#Part A

library(readxl)
toyota <- read_excel("Documents/GitHub/DataMining-HW1/Data-Mining/HW6/ToyotaCorolla.xlsx")

#converting Fule_Type to categorical variable
toyota$Fuel_Type <- factor(toyota$Fuel_Type)
toyota$Fuel_Type <- as.numeric(toyota$Fuel_Type)
toyota$Fuel_Type <- factor(toyota$Fuel_Type)

toyota<- toyota[c("Price","Age_08_04","KM","Fuel_Type","HP","Automatic","Doors","Quarterly_Tax","Mfr_Guarantee",
                  "Guarantee_Period","Airco","Automatic_airco","CD_Player","Powered_Windows","Sport_Model",
                  "Tow_Bar")]

#converting to dummy variable
library(caret)
dmy <- dummyVars("~.", data = toyota,fullRank = F)
toyota_d <- data.frame(predict(dmy, newdata = toyota))

#splitting
train_toyota<- toyota_d[1:1077,]
val_toyota<- toyota_d[1077:1436,]

#Build NN
library(neuralnet)
model <- neuralnet(
  formula = Price~Age_08_04+KM+Fuel_Type.1+Fuel_Type.2+Fuel_Type.3+HP+Automatic+Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+CD_Player+Powered_Windows+Sport_Model+Tow_Bar, 
  train_toyota,
  hidden = 1,
  threshold = 0.01,
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

#prediction
result <- prediction(model, list.glm = train_toyota$Price)

#Part A
#threshold values, 1, 0.1, 0.05, 0.01, 0.005, 0.001, and 0.0001
library(ModelMetrics)
rmse(train_toyota$Price, result)
rmse