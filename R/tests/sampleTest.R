set.seed(123)

# Generate independent variable (feature)
n <- 200  # Number of data points
x <- seq(1, 10, length.out = n)
y <- 2*x + rnorm(n, mean = 0, sd = 2)

data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]


# Generate dependent variable (target) with some noise
test_x = subset(testData, select = -y)
model = lm(y ~ ., data = trainData)
# preds = coef(model) %*% as.matrix(subset(testData, select = -y))
preds1 = predict(model, test_x)

rmse(preds1, testData$y)

head(preds1)

model = linear_regression(as.matrix(trainData$x), as.matrix(trainData$y))
model
preds = predict(model$fit, test_x)
rmse(preds, testData$y)

preds = predict_regression(model$coef, test_x, model$fit)
rmse(preds, testData$y)

res = bagging(trainData$x, trainData$y, testData, model_type = "linear", responseVariable = "y", R = 5)
res



# bagging

# Binomial
x <- matrix(rnorm(10000), ncol = 10)
y <- factor(sample(0:1, 1000, replace = TRUE))
data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "svm", responseVariable = "y", R = 10, type = "class", importance = T)
model_bagged
model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "logistic", responseVariable = "y", R = 10, type = "class", importance = T)
model_bagged


model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "svm", responseVariable = "y", R = 10, type = "class", importance = T)
model_bagged
model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "logistic", responseVariable = "y", R = 10, type = "class", importance = T)
model_bagged

# Multinomial

x <- matrix(rnorm(1500), ncol = 15)
y <- factor(sample(1:3, 100, replace = TRUE))

data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "svm", responseVariable = "y", R = 15, type = "class", importance = T)
model_bagged

# Regression
x <- matrix(rnorm(1000), ncol = 10)
y <- rnorm(100)
data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
model_bagged <- bagging(subset(trainData, select = -y), trainData$y, testData = testData, model_type = "linear", responseVariable = "y", R = 15, importance = F)
model_bagged

model = lm(y ~ ., data = trainData)
# preds = coef(model) %*% as.matrix(subset(testData, select = -y))
test_x = subset(testData, select = -y)
preds1 = predict(model, test_x)

rmse(preds1, testData$y)

###############################################################################
# ENSEMBLE

# Binomial
x <- matrix(rnorm(10000), ncol = 10)
y <- factor(sample(0:1, 1000, replace = TRUE))
data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
# model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = F, lambda = 0.1)
model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = T, k = 6)
model_bagged

# Multinomial Classification

x <- matrix(rnorm(1500), ncol = 15)
y <- factor(sample(1:3, 100, replace = TRUE))

data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = T, k = 6)
model_bagged

# Regression
x <- matrix(rnorm(1000), ncol = 10)
y <- rnorm(100)
data = data.frame(x, y)

trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
trainData <- data[trainID, ]
testData <- data[-trainID, ]
model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("svm","ridge","elastic_net"), responseVariable = "y", R = 15, importance = F)
model_bagged

model = lm(y ~ ., data = trainData)
# preds = coef(model) %*% as.matrix(subset(testData, select = -y))
test_x = subset(testData, select = -y)
preds1 = predict(model, test_x)

rmse(preds1, testData$y)

