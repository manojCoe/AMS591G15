# ===========================================
# svm
# ===========================================

# PATH = "C:/Users/coe16/Downloads/Mystery.csv"
PATH = "C:/Users/coe16/Downloads/Enigma.csv"

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
tune.control = tune.control(sampling = "cross", cross = 10)

# model = svmModel(data, responseVariable = "y", importance = TRUE, kernel = "radial")
model = svmModel(data, responseVariable = "y", importance = TRUE, kernel = "radial", type = "class")

testSet = convertCatToNumeric(subset(testData, select = -y), intercept = FALSE)
testSet = scale(testSet$data)

# Predict on the test set
# predictions <- predict(model, testSet, type = "response")
predictions <- predict(model, testSet, type = "classs")

# =========================
# For regression
preds = predict(model, testSet)
rmse(preds, testData$y)

# =========================
# For classification

preds = ifelse(predictions > 0.5, 1, 0)
# # predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
head(preds)

confusionMatrix(as.factor(testData$y), as.factor(preds))

# =======================================
# Using iris data

data("iris")
df = iris
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

x = subset(trainData, select = -Species)
# x = scale(x)
y = trainData$Species

data = cbind(x, y)
tune.control = tune.control(sampling = "cross", cross = 10)

model = svmModel(data, responseVariable = "y", importance = FALSE, kernel = "radial", type = "class")

testSet = convertCatToNumeric(subset(testData, select = -Species), intercept = FALSE)
testSet = scale(testSet$data)

predictions <- predict(model, testSet, type = "class")

# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)

confusionMatrix(as.factor(testData$Species), predictions)
