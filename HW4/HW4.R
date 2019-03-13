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
