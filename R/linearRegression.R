 # simple linear regression
slr <- function(x, y) {

    n <- length(x)

    x_mean <- mean(x)
    y_mean <- mean(y)

    # For calculating the slope beta1
    beta1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)

    # For calculating intercept b0
    beta0 <- y_mean - beta1 * x_mean

    coefficients = matrix(c(beta0, beta1), nrow = 1)

    # Return coefficients
    return(coefficients)
}

# Multiple Linear regression using normal equation
mlr <- function(x, y) {
    # get all the categorical columns in X
    categoricalColumns = sapply(x, is.character)

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        x = model.matrix(~ ., data = x)
    }

    if(!is.matrix(x)){
        print("Converting x to a matrix")
        x = as.matrix(x)
    }

    # Convert y to a matrix if it's a vector
    if (!is.matrix(y)) {
        print("Converting y to a matrix")
        y = as.matrix(y)
    }

    # Compute coefficients using the normal equation
    coefficients <- solve(t(x) %*% x) %*% t(x) %*% y

    # t(x): Transposes the model matrix x. This is done because in the normal equation, we need to multiply x by its transpose.
    #
    # %*%: Performs matrix multiplication. In this case, t(x) %*% x computes the cross product of x with its transpose, which is a critical step in the normal equation.
    #
    # solve(...): Computes the inverse of the matrix. In the normal equation formula (X^T * X)^(-1), this step computes the inverse of X^T * X.
    #
    # %*%: Performs matrix multiplication again. This time, it multiplies the result from the previous step by the transpose of x (t(x)).
    #
    # %*%: Finally, it multiplies the result from the previous step by y, the vector of response values, to get the coefficients of the linear regression model.

    # Return coefficients as a named vector or data frame
    return(as.data.frame(coefficients))
}

predict_regression <- function(coefficients, newdata) {

    # Check if coefficients is a matrix
    if (!is.matrix(coefficients)) {
        stop("Coefficients must be a matrix.")
    }

    # Add a column of ones for the intercept term if not present
    if (ncol(newdata) + 1 == nrow(coefficients)) {
        newdata <- cbind(1, newdata)
    }

    # Check if newdata is a matrix and convert if necessary
    if (!is.matrix(newdata)) {
        newdata <- as.matrix(newdata)
    }

    print(paste("class of coefficients: ", class(coefficients)))
    print(paste("class of newdata: ", class(newdata)))

    print(paste("dim of coef: ", dim(coefficients)))
    print(paste("dim of y: ", dim(newdata)))
    # Make predictions
    predictions <- newdata %*% coefficients

    # Convert predictions to a vector
    predictions <- as.vector(predictions)

    # Return predictions
    return(predictions)
}

linearRegression = function(formula, data = NULL){
    print(paste("data variable exists: ", !is.null(data)))
    formula_vars <- all.vars(formula)
    print(formula_vars)
    if(!is.null(data)){
        y_var = as.character(formula_vars[1])
        x_var = as.character(formula_vars[2])
        x = data[[x_var]]
        y = data[[y_var]]
    }
    else{
        df_var = as.character(formula_vars[1])
        y_var = as.character(formula_vars[2])
        x_var = as.character(formula_vars[3])
        data = get(df_var)
        x = data[[x_var]]
        y = data[[y_var]]
    }
    print(paste("x_var: ", x_var))
    print(paste("y_var: ", y_var))
    x = as.matrix(x)
    y = as.matrix(y)

    if(dim(x)[2] == 1){
        coefficients = slr(x,y)
    }
    else{
        coefficients = mlr(x,y)
    }
    return(c(intercept = coefficients[1], slope = coefficients[2]))
}

#
df = read.csv("C:/Users/coe16/Downloads/Mystery.csv")
#
set.seed(123)
trainID <- sample(1:nrow(df),round(0.75*nrow(df)))
trainData <- df[trainID,]
testData <- df[-trainID,]
#
#
# # formula1 = y ~ x1
coefficients = linearRegression(y~x2, data = trainData)
print(coefficients)
#
pred = predict_regression(coefficients = as.matrix(coefficients), newdata = as.matrix(testData$x2))
#
rmse(pred, testData$y)

model1 = lm(y ~ x2, data = trainData)
act <- predict(model1, subset(testData, select = x2))

rmse(act, testData$y)

#

X = subset(trainData, select = -y)
y = as.matrix(trainData$y)
coefficients3 = mlr(X, y)

testData$x15 = ifelse(testData$x15 == "y", 1, 0)

pred = predict_regression(coefficients = as.matrix(coefficients3), newdata = subset(testData, select = -y))
#
rmse(pred, testData$y)

model2 = lm(y ~ ., data = trainData)
act <- predict(model2, subset(testData, select = -y))

rmse(act, testData$y)

