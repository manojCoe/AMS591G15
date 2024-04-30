# multinomial
library(caret)
data("iris")
df = iris
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]
# testSet = preprocessTestData(subset(testData, select = -Species), intercept = FALSE)
x = model.matrix(Species ~ ., data = trainData)[, -1]
y = trainData$Species
# data = data.frame(x, y)
# y = as.factor(y)
res = bagging(x, y, testData = testData, responseVariable = "Species", model_type = "logistic", R = 10, type = "class", ignoreWarnings = T, importance = T)


# binomial

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

res = bagging(x, y, y~x, responseVariable = "y", testData = testData, model_type = "logistic", R = 20, type = "class", ignoreWarnings = T)
# testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)

confusionMatrix(as.factor(res$predictions), as.factor(testData$y))

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
# x = as.matrix(subset(trainData, select = -y))
x = model.matrix(y ~ ., data = trainData)[, -1]
y = trainData$y

# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "lasso", R = 20, ignoreWarnings = T, importance = T)
# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "elastic_net", R = 20, ignoreWarnings = T, importance = T)
res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "ridge", R = 20, ignoreWarnings = T, importance = T)
