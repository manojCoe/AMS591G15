# #
df = read.csv("C:/Users/coe16/Downloads/Mystery.csv")
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
coefficients = linearRegression(y~x2, data = trainData)
print(coefficients)
#
pred = predict_regression(coefficients = as.matrix(coefficients), newdata = as.matrix(testData$x2))
#
rmse(pred, testData$y)

model1 = lm(y ~ x2, data = trainData)
act <- predict(model1, subset(testData, select = x2))

rmse(act, testData$y)

#
c3 = linearRegression(y ~ ., trainData)
c3

testSet = subset(testData, select = -y)
testSet = convertCatToNumeric(testSet)

pred = predict_regression(coefficients = c3, newdata = testSet)
#
rmse(pred, testData$y)

testData <- df[-trainID,]

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

testSet = subset(testData, select = -y)
testSet = convertCatToNumeric(testSet)
testSet = as.matrix(testSet)

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

lambda = 0.1
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

# model = lassoRegression(x, y, lambda)
model = lassoRegression(subset(trainData, select = -y), trainData$y, lambda)

testSet = subset(testData, select = -y)
testSet = convertCatToNumeric(testSet)
testSet = as.matrix(testSet)

pred = predict_regression(model, testSet)

# Print the estimated coefficients
print(model)

library(MASS)
library(glmnet)
# Using glmnet function to verify
lasso_ <- glmnet(x,y, alpha = 1, lambda = lambda)

lmodel <- coef(lasso_)

print(lmodel)

act <- testSet %*% lmodel

rmse(pred, testData$y)
rmse(act, testData$y)

# ===========================================
# ElasticNet Regression
# ===========================================
lambda1 = 0.1
lambda2 = 0.2
model = elasticNet(subset(trainData, select = -y), trainData$y, lambda1, lambda2)
print(model)

testSet = subset(testData, select = -y)
testSet = convertCatToNumeric(testSet)
testSet = as.matrix(testSet)

pred = predict_regression(model, testSet)

# Print the estimated coefficients

library(MASS)
library(glmnet)
x = model.matrix(y ~ ., data = trainData)
y = trainData$y

# Using glmnet function to verify
elastic_ <- glmnet(x,y, alpha = 0.5, lambda = lambda2)

emodel <- coef(elastic_)

print(emodel)

act <- testSet %*% lmodel

rmse(pred, testData$y)
rmse(act, testData$y)

# ===========================================
# Logistic Regression
# ===========================================
library(caret)
df = read.csv("C:/Users/MSP/Downloads/Unknown.csv", header = T)
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

testSet = subset(testData, select = -y)
testSet = cbind(1, testSet)
# testSet = as.matrix(testSet)

x = as.matrix(subset(trainData, select = -y))
y = as.matrix(trainData$y)
beta = numeric(13)
model = logisticRegression(x, y, epochs = 1000)
model
preds = as.matrix(testSet) %*% as.matrix(model)
probabilities_test <- 1 / (1 + exp(-preds))
head(probabilities_test)
predictions_test <- ifelse(probabilities_test > 0.5, 1, 0)

table(predictions_test, testData$y)
y_act = as.matrix(testData$y)

confusionMatrix(data = factor(predictions_test, levels = c(0, 1)),
                reference = factor(y_act, levels = c(0, 1)))

x = model.matrix(y ~ ., data = trainData)
y = trainData$y

logModel <- glm(y ~ ., data = trainData, family = binomial)
logModel$coefficients

preds = predict(logModel, subset(testData, select = -y), type = "response")
act = ifelse(preds<0.5, 0, 1)
# table(act, testData$y)

confusionMatrix(data = factor(act, levels = c(0, 1)),
                reference = factor(y_act, levels = c(0, 1)))
