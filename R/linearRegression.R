 # simple linear regression
slr <- function(x, y) {
    print("Inside slr()")

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
    print("Inside mlr()")

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
    print(head(x))

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

linearRegression = function(formula, data = NULL){
    print("Inside linearRegression()")
    print(paste("data variable exists: ", !is.null(data)))
    formula_vars <- all.vars(formula)
    print(formula_vars)

    # If data variable is not null
    if(!is.null(data)){
        y_var = as.character(formula_vars[1])
        x_var = as.character(formula_vars[2])

        # if x_var == ., then consider all the predictor columns in training data.
        if(x_var == "."){
            print("consider all the predictor columns in training data")
            x = subset(data, select = -y)
        }
        else{
            x = data[[x_var]]
        }
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

    # print(paste("dim of x: ", dim(x)))
    # print(paste("dim of y: ", dim(y)))

    if(dim(x)[2] != 1){
        coefficients = mlr(x,y)
        return(coefficients)
    }
    else{
        x = as.matrix(x)
        y = as.matrix(y)
        coefficients = slr(x,y)
        return(c(intercept = coefficients[1], slope = coefficients[2]))
    }
}


