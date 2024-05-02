PATH = "C:/Users/MSP/Downloads/Mystery.csv"
df = read.csv(PATH)
df = na.omit(df)
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]

# x = convertCatToNumeric(subset(trainData, select = -y), intercept = FALSE)$data
x = model.matrix(y ~ ., data = trainData)[, -1]
y = as.matrix(trainData$y)

model = linear_regression(x, trainData$y, importance = TRUE, alpha = 0)
model

#
# testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE, features = rownames(model$coef)[-1])
testSet = preprocessTestData(subset(testData, select = -y), intercept = TRUE)
test_x = as.matrix(testSet[, model$selectedFeatures])


pred = predict_regression(model$coef, test_x)
rmse(pred, testData$y)
