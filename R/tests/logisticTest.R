library(caret)
PATH = "C:/Users/MSP/Downloads/Enigma.csv"
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

testSet = preprocessTestData(subset(testData, select = -y), intercept = FALSE)
test_x = as.matrix(testSet)

model = logistic_regression(x,y,type = 'class', importance = TRUE)

# model = logisticRegression(x, y, FALSE, FALSE, 1)
# Predict on the test set
predictions <- predict(model$fit, test_x, type = "class")
# predictions <- predict(model, newx = test_x, s = "lambda.min", type = "response")
# preds = ifelse(predictions > 0.5, 1, 0)
# predictions =  factor(predictions, levels = 1:2, labels = c(0, 1))
head(predictions)
# Compute accuracy
library(caret)
head(preds)

confusionMatrix(as.factor(predictions), as.factor(testData$y))
