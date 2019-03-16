#HW 4
#Connor Rouan and Yadukrishnan Sethumadhavan

#Problem 1
#remove ID/zip code columns
train <- UniversalBank_train
test <- UniversalBank_val
cl_train <- train$personal_loan
cl_test <- test$personal_loan
train$ID <- NULL
test$ID <- NULL
train$zip <- NULL
test$zip <- NULL
train$personal_loan <- NULL
test$personal_loan <- NULL

#Create dummy variables
library(caret)
test$education <- factor(test$education)
dmy <- dummyVars("~.", data = test,fullRank = F)
test <- data.frame(predict(dmy, newdata = test))
train$education <- factor(train$education)
dmy <- dummyVars("~.", data = train,fullRank = F)
train <- data.frame(predict(dmy, newdata = train))

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
train_n <- as.data.frame(lapply(train, normalize))
test_n <- as.data.frame(lapply(test, normalize))

#create model
library(class)
model <- knn(train_n, test_n, cl_train, k=1)

#test performance
library(gmodels)
CrossTable(x=cl_test, y=model)

#Part A
predict <- data.frame(40,10,84,2,2,1,1,0,0,0,0,1,1)
names(predict)<-c("age",	"experience","income", "family",	"CC_avg", "education.1","education.2","education.3",	"mortgage", "securities_account", "CD_account",	"online", "credit_card")
train <- UniversalBank_train
cl_train <- train$personal_loan
train$ID <- NULL
train$zip <- NULL
train$personal_loan <- NULL

#Create dummy variables
library(caret)
train$education <- factor(train$education)
dmy <- dummyVars("~.", data = train,fullRank = F)
train <- data.frame(predict(dmy, newdata = train))

combined <- rbind(train, predict)

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
combined_n <- as.data.frame(lapply(combined, normalize))

train_n <- head(combined_n,3000)
predict_n <- tail(combined_n,1)
cl_predict <- 0

#create model
library(class)
model <- knn(train_n, predict_n, cl_train, k=1)
print(model)


#Part B
#remove ID/zip code columns
train <- UniversalBank_train
test <- UniversalBank_val
cl_train <- train$personal_loan
cl_test <- test$personal_loan
train$ID <- NULL
test$ID <- NULL
train$zip <- NULL
test$zip <- NULL
train$personal_loan <- NULL
test$personal_loan <- NULL

#Create dummy variables
library(caret)
test$education <- factor(test$education)
dmy <- dummyVars("~.", data = test,fullRank = F)
test <- data.frame(predict(dmy, newdata = test))
train$education <- factor(train$education)
dmy <- dummyVars("~.", data = train,fullRank = F)
train <- data.frame(predict(dmy, newdata = train))

#create model
library(class)
result <- 0
best_k <- 0
for (i in 1:20) {
  model <- knn(train, test, cl_train, k=i)
  new_result = 100*sum(cl_test==model)/2000
  if (new_result > result) {
    result <- new_result
    best_k <- i
  }
}
print(best_k)

#Part C
#create model
library(class)
model <- knn(train, test, cl_train, k=15)

#test performance
library(gmodels)
CrossTable(x=cl_test, y=model)

#Part D
predict <- data.frame(40,10,84,2,2,1,1,0,0,0,0,1,1)
names(predict)<-c("age",	"experience","income", "family",	"CC_avg", "education.1","education.2","education.3",	"mortgage", "securities_account", "CD_account",	"online", "credit_card")
train <- UniversalBank_train
cl_train <- train$personal_loan
train$ID <- NULL
train$zip <- NULL
train$personal_loan <- NULL

#Create dummy variables
library(caret)
train$education <- factor(train$education)
dmy <- dummyVars("~.", data = train,fullRank = F)
train <- data.frame(predict(dmy, newdata = train))

combined <- rbind(train, predict)

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
combined_n <- as.data.frame(lapply(combined, normalize))

train_n <- head(combined_n,3000)
predict_n <- tail(combined_n,1)
cl_predict <- 0

#create model
library(class)
model <- knn(train_n, predict_n, cl_train, k=15)
print(model)

#Part E
#remove ID/zip code columns
train <- UniversalBank_50
val <- UniversalBank_30
test <- UniversalBank_20
cl_train <- train$personal_loan
cl_val <- val$personal_loan
cl_test <- test$personal_loan
train$ID <- NULL
val$ID <- NULL
test$ID <- NULL
train$zip <- NULL
val$zip <- NULL
test$zip <- NULL
train$personal_loan <- NULL
val$personal_loan <- NULL
test$personal_loan <- NULL

#Create dummy variables
library(caret)
train$education <- factor(train$education)
dmy <- dummyVars("~.", data = train,fullRank = F)
train <- data.frame(predict(dmy, newdata = train))
val$education <- factor(val$education)
dmy <- dummyVars("~.", data = val,fullRank = F)
val <- data.frame(predict(dmy, newdata = val))
test$education <- factor(test$education)
dmy <- dummyVars("~.", data = test,fullRank = F)
test <- data.frame(predict(dmy, newdata = test))

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
train_n <- as.data.frame(lapply(train, normalize))
val_n <- as.data.frame(lapply(val, normalize))
test_n <- as.data.frame(lapply(test, normalize))

#create models
library(class)
model_val <- knn(train_n, val_n, cl_train, k=15)
model_test <- knn(train_n, test_n, cl_train, k=15)

#test performance
library(gmodels)
CrossTable(x=cl_val, y=model_val)
CrossTable(x=cl_test, y=model_test)


#Problem 2
#Part A
train <- BostonHousing_train
test <- BostonHousing_test
cl_train <- train$MEDV
cl_test <- test$MEDV
train$CAT_MEDV <- NULL
test$CAT_MEDV <- NULL
train$MEDV <- NULL
test$MEDV <- NULL

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
train_n <- as.data.frame(lapply(train, normalize))
test_n <- as.data.frame(lapply(test, normalize))

#create model
library(class)
result <- 0
best_k <- 0
for (i in 1:5) {
  model <- knn(train_n, test_n, cl_train, k=i)
  new_result = 100*sum(cl_test==model)/2000
  if (new_result > result) {
    result <- new_result
    best_k <- i
  }
}
print(best_k)

#Part B
predict <- data.frame(.2,0,7,0,.538,6,62,4.7,4,307,21,10)
names(predict)<-c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "LSTAT")
train <- BostonHousing_train
cl_train <- train$MEDV
train$CAT_MEDV <- NULL
train$MEDV <- NULL
combined <- rbind(train, predict)

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
combined_n <- as.data.frame(lapply(combined, normalize))

train_n <- head(combined_n,303)
predict_n <- tail(combined_n,1)
cl_predict <- 0

#create model
library(class)
model <- knn(train_n, predict_n, cl_train, k=4)
print(model)

#Part C
train <- BostonHousing_train
cl_train <- train$MEDV
train$CAT_MEDV <- NULL
train$MEDV <- NULL

#normalize the data
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
train_n <- as.data.frame(lapply(train, normalize))

#create model
model <- knn(train_n, train_n, cl_train, k=4)

#test performance
library(gmodels)
CrossTable(x=cl_train, y=model)

#Problem 3
Accidents$INJURY <- 0
for(i in 1:nrow(Accidents)) {
  if(Accidents[i,]$MAX_SEV_IR == 0) {
    Accidents[i,]$INJURY = 0
  }
  else {
    Accidents[i,]$INJURY = 1
  }
}
