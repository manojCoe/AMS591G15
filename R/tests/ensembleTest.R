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


res = ensemble(x, y, testData = testData, models = c("logistic", "elastic_net", "lasso"), bagging = T, responseVariable = "Species", R = 10, type = "class", ignoreWarnings = F, importance = T, nfolds = 10)
res
# confusionMatrix(as.factor(res[[1]]), testData$Species)
# confusionMatrix(as.factor(res[[2]]), testData$Species)
confusionMatrix(as.factor(res$predictions), as.factor(testData$Species))
confusionMatrix(as.factor(res$weightedPredictions), as.factor(testData$Species))
# getAccuracyForClassification(res$predictions, testData$Species)

# data = data.frame(x, y)
# y = as.factor(y)
# res = bagging(x, y, testData = testData, responseVariable = "Species", model_type = "logistic", R = 10, type = "class", ignoreWarnings = T, importance = T)


# binomial

library(caret)
PATH = "C:/Users/coe16/Downloads/Enigma.csv"
df = read.csv(PATH)

df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]



head(trainData)

x = as.matrix(subset(trainData, select = -y))
y = as.factor(trainData$y)

res = ensemble(x, y, testData = testData, models = c("logistic", "svm", "lasso", "elastic_net"), bagging = F, responseVariable = "y", R = 10, type = "class", ignoreWarnings = T, importance = T, nfolds = 10)
confusionMatrix(as.factor(res$predictions), as.factor(testData$y))
confusionMatrix(as.factor(res$weightedPredictions), as.factor(testData$y))
# confusionMatrix(as.factor(res[[2]]), as.factor(testData$y))
res

actModel = glmnet(x, y, family = "binomial", lambda = 0.1)
testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
preds = predict(actModel, testSet, type = "class")
confusionMatrix(as.factor(preds), as.factor(testData$y))


# Regression

PATH = "C:/Users/coe16/Downloads/Mystery.csv"
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

res = ensemble(x, y, testData = testData, models = c("linear", "svm"), bagging = F, responseVariable = "y", R = 10, ignoreWarnings = T, importance = F, nfolds = 10)
res

rmse(res$predictions, testData$y)
rmse(res$weightedPredictions, testData$y)
# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "lasso", R = 20, ignoreWarnings = T, importance = T)
# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "elastic_net", R = 20, ignoreWarnings = T, importance = T)
