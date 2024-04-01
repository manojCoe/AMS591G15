predict_regression <- function(coefficients, newdata) {

    # Check if coefficients is a matrix
    if (is.data.frame(coefficients)) {
        coefficients <- as.matrix(coefficients)
    }


    # Add a column of ones for the intercept term if not present
    if (ncol(newdata) + 1 == nrow(coefficients)) {
        newdata <- cbind(1, newdata)
    }
    # Check if y is a data frame and convert it to a matrix if necessary
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

