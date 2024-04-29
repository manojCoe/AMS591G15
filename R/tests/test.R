# #
# Change the path
PATH = "C:/Users/MSP/Downloads/Mystery.csv"
df = read.csv(PATH)
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]
#
#

# ===========================================
# Linear Regression
# ===========================================

# # formula1 = y ~ x1

# For no intercept form
# coefficients = linearRegression(y~x2, data = trainData, intercept = FALSE)
# print(coefficients)

# With intercept
coefficients = linearRegression(y~x2, data = trainData)
print(coefficients)
#
pred = predict_regression(coefficients = as.matrix(coefficients), newdata = as.matrix(testData$x2))
#
rmse(pred, testData$y)

# model1 = lm(y ~ 0 + x2, data = trainData) # For no intercept
model1 = lm(y ~ x2, data = trainData)
act <- predict(model1, subset(testData, select = x2))

rmse(act, testData$y)

#
# c3 = linearRegression(y ~ ., trainData, intercept = FALSE) # For no intercept
c3 = linearRegression(y ~ ., trainData)
c3

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
# testSet = convertCatToNumeric(testSet)

pred = predict_regression(coefficients = c3, newdata = testSet)
#
rmse(pred, testData$y)

testData <- df[-trainID,]

# model2 = lm(y ~ 0 + ., data = trainData) # For no intercept term
model2 = lm(y ~ ., data = trainData)
act <- predict(model2, subset(testData, select = -y))

rmse(act, testData$y)

# ===========================================
# Ridge Regression
# ===========================================

lambda = 0.1
x = subset(trainData, select = -y)
y = subset(trainData, select = y)

model = ridgeRegression(x, y, lambda)

print(model)

testSet = preprocessTestData(subset(testData, select = -y))
# testSet = convertCatToNumeric(testSet)
# testSet = as.matrix(testSet)

pred = predict_regression(model, testSet)
#
library(glmnet)
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

# Using glmnet function to verify

ridge_ = glmnet(x,y, alpha = 0, lambda = lambda)
rmodel = coef(ridge_)
print(rmodel)
#

act <- testSet %*% rmodel

rmse(pred, testData$y)
rmse(act, testData$y)

# ===========================================
# Lasso Regression
# ===========================================

PATH = "C:/Users/MSP/Downloads/Mystery.csv"
df = read.csv(PATH)
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

# x = subset(trainData, select = -y)
x = convertCatToNumeric(subset(trainData, select = -y), intercept = FALSE)$data
y = as.matrix(trainData$y)

# cv.fit <- crossValidation(x, y, alpha = 1, nfolds = 5)
# cvModel = glmnet(x[, cv.fit$features], y, alpha = 1, lambda = cv.fit$bestLambda)

# model = lasso_regression(x, trainData$y)
model = lasso_regression(x, trainData$y, importance = TRUE)
model

#
testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE, features = model$features)



pred = predict_regression(model$coef, testSet)
rmse(pred, testData$y)

library(caret)
PATH = "C:/Users/MSP/Downloads/Enigma.csv"
df = read.csv(PATH)

df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(trainData)

x = as.matrix(subset(trainData, select = -y))
y = trainData$y

model = lasso_regression(x, y, importance = FALSE, type = "class")
testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)

predictions = predict(model$fit, testSet, type = "class")
preds = ifelse(predictions > 0.5, 1, 0)
head(preds)

confusionMatrix(as.factor(preds), as.factor(testData$y))

# lambda = 0.1
# x = model.matrix(y ~ ., data = trainData)[, -1]
# y = trainData$y
#
# # model = lassoRegression(x, y, lambda)
# # model = lassoRegression(subset(trainData, select = -y), trainData$y, lambda)
# model = lassoRegression(x, y, lambda)
#
# testSet = preprocessTestData(subset(testData, select = -y))
#
# pred = predict_regression(model, testSet)
#
# # Print the estimated coefficients
# print(model)
#
library(MASS)
library(glmnet)
# Using glmnet function to verify
lasso_ <- glmnet(x,y, alpha = 1, lambda = 0.01)

lmodel <- coef(lasso_)

print(lmodel)

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)

act <- predict_regression(lmodel, testSet)

rmse(pred, testData$y)
rmse(act, testData$y)

# ===========================================
# ElasticNet Regression
# ===========================================
lambda1 = 0.1
lambda2 = 0.2

x = convertCatToNumeric(subset(trainData, select = -y), intercept = FALSE)$data
y = as.matrix(trainData$y)

actModel = cv.glmnet(x, y, relax = FALSE, alpha = 0.5)
getInformativePredictors(actModel)
coef(actModel)
lambda = actModel$lambda.min

PATH = "C:/Users/MSP/Downloads/Mystery.csv"
df = read.csv(PATH)
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

# x = subset(trainData, select = -y)
x = convertCatToNumeric(subset(trainData, select = -y), intercept = FALSE)$data
y = as.matrix(trainData$y)

cv.fit <- crossValidation(x, y, alpha = 1, nfolds = 5)
cvModel = glmnet(x[, cv.fit$features], y, alpha = 0.5, lambda = cv.fit$bestLambda)

model = elastic_net_regression(x, trainData$y)
# model = elastic_net_regression(x, trainData$y, importance = TRUE)
model


coefficients_all <- as.matrix(coef(model))

# Sum the coefficients across all classes
coefficients_sum <- Reduce(`+`, coefficients_all)
coefficients_matrix <- as.matrix(coefficients_sum)[-1, , drop=FALSE]

# Extract the absolute values of coefficients
coefficients_matrix_without_intercept <- coefficients_matrix[-1, , drop = FALSE]

selected_vars <- which(coefficients_matrix_without_intercept != 0)

# model = elasticNet(subset(trainData, select = -y), trainData$y, lambda1, lambda2)
# print(model)
#
testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE)

pred = predict_regression(coef(model), testSet)
rmse(pred, testData$y)

library(caret)
PATH = "C:/Users/MSP/Downloads/Enigma.csv"
df = read.csv(PATH)

df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(trainData)

x = as.matrix(subset(trainData, select = -y))
y = trainData$y

model = elastic_net_regression(x, y, cv = TRUE, type = "class")
testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)

predictions = predict(model, testSet, type = "class")
preds = ifelse(predictions > 0.5, 1, 0)
head(preds)

confusionMatrix(as.factor(preds), as.factor(testData$y))
# Print the estimated coefficients

library(MASS)
library(glmnet)
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

# Using glmnet function to verify
elastic_ <- glmnet(x,y, alpha = 0.5, lambda = 0.2)

emodel <- coef(elastic_)

print(emodel)

act <- testSet %*% lmodel

rmse(pred, testData$y)
rmse(act, testData$y)

# ===========================================
# Logistic Regression
# ===========================================

PATH = "C:/Users/MSP/Downloads/Enigma.csv"
df = read.csv(PATH)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(df)
# str(df)
lambda = 0.01
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
test_x = as.matrix(testSet)

model = logisticRegression(x, y, FALSE, FALSE, 1)
# Predict on the test set
predictions <- predict(model, test_x, type = "class")
# predictions <- predict(model, newx = test_x, s = "lambda.min", type = "response")
# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
head(preds)

confusionMatrix(as.factor(predictions), as.factor(testData$y))

# ===========================================
# SVM
# ===========================================

PATH = "C:/Users/MSP/Downloads/Enigma.csv"
df = read.csv(PATH)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(df)
# str(df)
lambda = 0.01
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
test_x = as.matrix(testSet)

model = svmModel(x, y, FALSE, kernel = "radial", cost = 1)
# Predict on the test set
predictions <- predict(model, test_x, type = "class")
# predictions <- predict(model, newx = test_x, s = "lambda.min", type = "response")
# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
head(preds)

confusionMatrix(as.factor(predictions), as.factor(testData$y))

# ===========================================
# bagging
# ===========================================
data("iris")
head(iris)

# x = subset(iris, select = -Species)
# y = as.matrix(iris$Species)

x = model.matrix(Species ~ ., data = iris)[, -1]
y = iris$Species
data = data.frame(x, y)
y = as.matrix(y)
res = bagging(x, y, y~x, "logistic", 50, "class", lambda = 0.01, bagging_type = "majority_vote")


# ===========================================
# cross validation
# ===========================================
PATH = "C:/Users/MSP/Downloads/Mystery.csv"
# PATH = "C:/Users/MSP/Downloads/Enigma.csv"
df = read.csv(PATH)

df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(trainData)

x = subset(trainData, select = -y)
y = trainData$y
cv.fit = crossValidation(subset(trainData, select = -y), y)
# cv.fit = crossValidation(subset(trainData, select = -y), y, type = "class")
