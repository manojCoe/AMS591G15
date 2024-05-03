library(caret)
PATH = "C:/Users/Prasad/Downloads/Mystery.csv"
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


model = logistic_regression(x,y,type = 'class', importance = F)
testSet = preprocessTestData(subset(testData, select = -y), intercept = F, features = model$selectedFeatures)
test_x = as.matrix(testSet)

# model = logisticRegression(x, y, FALSE, FALSE, 1)
# Predict on the test set
predictions <- predict(model$fit, testSet, type = "class")
# predictions <- predict(model, newx = test_x, s = "lambda.min", type = "response")
# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
# head(preds)

confusionMatrix(as.factor(predictions), as.factor(testData$y))


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
model = logistic_regression(x,y,type = 'class', importance = F)
testSet = preprocessTestData(subset(testData, select = -Species), intercept = F, features = model$selectedFeatures)
test_x = as.matrix(testSet)

# model = logisticRegression(x, y, FALSE, FALSE, 1)
# Predict on the test set
predictions <- predict(model$fit, testSet, type = "class")
# predictions <- predict(model, newx = test_x, s = "lambda.min", type = "response")
# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
# head(preds)

confusionMatrix(as.factor(predictions), as.factor(testData$Species))

