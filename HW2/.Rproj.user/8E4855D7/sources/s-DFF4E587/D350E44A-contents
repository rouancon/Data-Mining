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
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
par(mfrow=c(2,2))
plot(fit)

#Enhanced Approach
library(car)
par(mfrow=c(1,1))
fit <- lm(formula=Compressive_Strength ~ I(Cement^2) + Water + I(Coarse_Aggregate^2) + Fly_Ash, data=concrete)
qqPlot(fit, labels=row.names(concrete), id.method="identify", simulate=TRUE, main="Q-Q Plot")

library(car)
par(mfrow=c(2,2))
qqPlot(fit, labels=row.names(concrete), id.method="identify", simulate=TRUE, main="Q-Q Plot")

#Part D