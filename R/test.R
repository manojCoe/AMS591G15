#
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

pred = predict_regression(coefficients = as.matrix(c3), newdata = testSet)
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

# Print the estimated coefficients
print(model)

library(MASS)
# Perform ridge regression using the lm.ridge function
ridge_result <- lm.ridge(y ~ ., data = trainData, lambda = lambda)

# Extract the coefficients from the lm.ridge result
ridgeModel <- coef(ridge_result)

print(ridgeModel)

testData$x15 = ifelse(testData$x15 == "y", 1, 0)

pred = predict_regression(model, newdata = subset(testData, select = -y))

testData <- df[-trainID,]

# ref: https://stackoverflow.com/questions/37895372/predictions-of-ridge-regression-in-r

testSet = subset(testData, select = -y)
testSet = convertCatToNumeric(testSet)
testSet = as.matrix(testSet)
act <- testSet %*% ridgeModel

rmse(pred, testData$y)
rmse(act, testData$y)

