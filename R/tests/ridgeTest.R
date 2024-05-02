# Regression
PATH = "C:/Users/MSP/Downloads/Mystery.csv"
df = read.csv(PATH)
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

lambda = 0.1
x = convertCatToNumeric(subset(trainData, select = -y), intercept = FALSE)$data
y = as.matrix(trainData$y)

model = ridge_regression(x, y, importance = TRUE)
# model = ridgeRegression(x, y, lambda)

print(model)

testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE)
test_x = as.matrix(testSet[, model$selectedFeatures])


pred = predict_regression(model$coef, test_x)
rmse(pred, testData$y)


# Classification

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

model = ridge_regression(x, y, importance = TRUE, type = "class")
testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE)

predictions = predict(model$fit, testSet, type = "class")
head(predictions)

confusionMatrix(as.factor(predictions), as.factor(testData$y))

