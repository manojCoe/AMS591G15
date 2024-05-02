# ===========================================
# svm
# ===========================================
library(e1071)

# PATH = "C:/Users/MSP/Downloads/Mystery.csv"
PATH = "C:/Users/MSP/Downloads/Enigma.csv"

df = read.csv(PATH)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

head(df)
# str(df)
x = subset(trainData, select = -y)
# x = scale(x)
y = trainData$y

data = cbind(x, y)

# model = svmModel(data, responseVariable = "y", importance = FALSE, kernel = "radial")
model = svmModel(data, responseVariable = "y", importance = F, kernel = "radial", type = "class")

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
testSet = scale(testSet)
test_x = testSet[, model$selectedFeatures]
# Predict on the test set
# predictions <- predict(model, testSet, type = "response")
# predictions <- predict(model, testSet, type = "class")

# =========================
# For regression
preds = predict(model$fit, test_x)
rmse(preds, testData$y)

# =========================
# For classification
predictions <- predict(model$fit, testSet, type = "class")
head(predictions)
# preds = ifelse(predictions > 0.5, 1, 0)
# # predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
# head(preds)

confusionMatrix(as.factor(testData$y), as.factor(predictions))

# =======================================
# Using iris data

data("iris")
df = iris
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

x = as.matrix(subset(trainData, select = -Species))
# x = scale(x)
y = trainData$Species

data = data.frame(x, y)
tune.control = tune.control(sampling = "cross", cross = 10)

model = svmModel(data, responseVariable = "y", importance = TRUE, kernel = "radial", type = "class")

testSet = convertCatToNumeric(subset(testData, select = -Species), intercept = FALSE)
testSet = scale(testSet$data)

predictions <- predict(model$fit, testSet, type = "class")

# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)

confusionMatrix(as.factor(testData$Species), as.factor(predictions))
