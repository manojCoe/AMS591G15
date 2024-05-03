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


res = ensemble(x, y, testData = testData, models = c("logistic", "svm"), bagging = F, responseVariable = "Species", R = 10, type = "class", ignoreWarnings = F, importance = F, nfolds = 10, lambda = 0.1)
res
# confusionMatrix(as.factor(res[[1]]), testData$Species)
# confusionMatrix(as.factor(res[[2]]), testData$Species)

# data = data.frame(x, y)
# y = as.factor(y)
# res = bagging(x, y, testData = testData, responseVariable = "Species", model_type = "logistic", R = 10, type = "class", ignoreWarnings = T, importance = T)


# binomial

library(caret)
PATH = "C:/Users/Prasad/Downloads/Mystery.csv"
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

res = ensemble(x, y, testData = testData, models = c("logistic", "svm", "lasso"), bagging = F, responseVariable = "y", R = 10, type = "class", ignoreWarnings = T, importance = T, nfolds = 10)
confusionMatrix(as.factor(res$predictions), as.factor(testData$y))
# confusionMatrix(as.factor(res[[2]]), as.factor(testData$y))
res

actModel = glmnet(x, y, family = "binomial")
testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
preds = predict(actModel, testSet, type = "class")
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

res = ensemble(x, y, testData = testData, models = c("linear", "svm"), bagging = F, responseVariable = "y", R = 10, ignoreWarnings = T, importance = T, nfolds = 10)
res


# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "lasso", R = 20, ignoreWarnings = T, importance = T)
# res = bagging(x, y, responseVariable = "y", testData = testData, model_type = "elastic_net", R = 20, ignoreWarnings = T, importance = T)
