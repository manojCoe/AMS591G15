#
df = read.csv("C:/Users/coe16/Downloads/Mystery.csv")
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]
#
#
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

X = subset(trainData, select = -y)
y = as.matrix(trainData$y)
coefficients3 = mlr(X, y)

testData$x15 = ifelse(testData$x15 == "y", 1, 0)

pred = predict_regression(coefficients = as.matrix(coefficients3), newdata = subset(testData, select = -y))
#
rmse(pred, testData$y)

testData <- df[-trainID,]

model2 = lm(y ~ ., data = trainData)
act <- predict(model2, subset(testData, select = -y))

rmse(act, testData$y)
