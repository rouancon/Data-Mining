#HW 5
#Connor Rouan and Yadukrishnan Sethumadhavan

#Problem 1

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
traintoyota<- toyota_n[1:718,]
valtoyota<- toyota_n[719:1149,]
testtoyota<-toyota_n[1149:1436,]
