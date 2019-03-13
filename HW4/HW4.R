#HW 3
#Connor Rouan and Yadukrishnan Sethumadhavan

#Task 3
#Part A
Concrete_Slump_Test_Data$No <- NULL
concrete <- as.data.frame(Concrete_Slump_Test_Data)
library(car)
scatterplotMatrix(concrete, spread=FALSE, lty.smooth=2, main="Scatter Plot Matrix")

cor(concrete)
model <- lm(formula=Compressive_Strength ~., data=concrete)
summary(model)

#Part B
model <- lm(formula=Compressive_Strength ~ Cement + Water, data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ Cement + I(Water^2), data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water, data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ Cement + Water + Coarse_Aggregate + Fly_Ash, data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ Cement + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + Coarse_Aggregate + Fly_Ash, data=concrete)
summary(model)

model <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
summary(model)

#Part C
#Typical Approach
#Model 1
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
par(mfrow=c(2,2))
plot(fit)

#Model 2
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + Coarse_Aggregate + Fly_Ash, data=concrete)
par(mfrow=c(2,2))
plot(fit)

#Enhanced Approach
#Model 1
library(car)
par(mfrow=c(1,1))
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
qqPlot(fit, labels=row.names(concrete), id.method="identify", simulate=TRUE, main="Q-Q Plot")

library(car)
par(mfrow=c(2,2))
crPlots(fit)

#Model 2
library(car)
par(mfrow=c(1,1))
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + Coarse_Aggregate + Fly_Ash, data=concrete)
qqPlot(fit, labels=row.names(concrete), id.method="identify", simulate=TRUE, main="Q-Q Plot")

library(car)
par(mfrow=c(2,2))
crPlots(fit)

#Part D

#Task 4
#Part A
Forest_Fires_Data$Month <- NULL
Forest_Fires_Data$Day <- NULL
fires <- as.data.frame(Forest_Fires_Data)
library(car)
scatterplotMatrix(fires, spread=FALSE, lty.smooth=2, main="Scatter Plot Matrix")

cor(fires)
model <- lm(formula=Area ~., data=fires)
summary(model)

#Part B
model <- lm(formula=Area ~ X + DMC + ISI + Temp + RH + Wind, data=fires)
model[["model"]][["Area"]] <- log(model[["model"]][["Area"]])
summary(model)

model <- lm(formula=Area ~ X + DMC + ISI + Temp + RH + Wind, data=fires)
summary(model)

model <- lm(formula=Area ~ log(X + DMC + ISI + Temp + RH + Wind), data=fires)
summary(model)

model <- lm(formula=Area ~ I(Rain^2) + FFMC + DC, data=fires)
summary(model)

plot(Forest_Fires_Data$Area, model$fitted.values)

model <- lm(formula=Area ~ FFMC + DC, data=fires)
summary(model)

model <- lm(formula=Area ~ Rain + model$fitted.values, data=fires)
summary(model)

plot(Forest_Fires_Data$Area, model$fitted.values)

#Part C
#Typical Approach
fit <- lm(formula=Area ~ Rain + FFMC + DC, data=fires)
par(mfrow=c(2,2))
plot(fit)

#Enhanced Approach
library(car)
par(mfrow=c(1,1))
fit <- lm(formula=Area ~ Rain + FFMC + DC, data=fires)
qqPlot(fit, labels=row.names(fires), id.method="identify", simulate=TRUE, main="Q-Q Plot")

library(car)
par(mfrow=c(2,2))
crPlots(fit)

#Part D

