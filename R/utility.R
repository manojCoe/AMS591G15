convertCatToNumeric = function(x, intercept = TRUE){
    # print("Inside convertCatToNumeric()")
    categoricalColumns = sapply(x, is.character)

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        print("X contains categorical data.")
        modelMatrix = model.matrix(~ ., data = x)
        if(intercept == FALSE){
            x = modelMatrix[, -1]
        }
        else{
            x = modelMatrix
        }
    }
    # print("executed convertCatToNumeric()")
    return(x)
}

rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}

softThreshold <- function(x, lambda) {
    result = sign(x) * max(abs(x) - lambda, 0)
}

# Sigmoid function
sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
}
