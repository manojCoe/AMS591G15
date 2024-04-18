convertCatToNumeric = function(x, intercept = TRUE){
    print("Inside convertCatToNumeric()")
    categoricalColumns = sapply(x, is.character)
    hasCategorical = FALSE

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        print("X contains categorical data.")
        hasCategorical = TRUE
        modelMatrix = model.matrix(~ ., data = x)
        if(intercept == FALSE){
            x = modelMatrix[, -1]
        }
        else{
            x = modelMatrix
        }
    }
    print("executed convertCatToNumeric()")
    return(list(data = x, hasCategorical = hasCategorical))
}

rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}

softThreshold = function(x, lambda) {
    result = sign(x) * max(abs(x) - lambda, 0)
}

preprocessTestData = function(testSet, intercept = TRUE){
    x = convertCatToNumeric(testSet, intercept)
    hasCategorical = x$hasCategorical
    x = x$data

    if(!hasCategorical & intercept){
        print("Adding intercept column to matrix.")
        x = cbind(1, x)
    }
    x = as.matrix(x)
    return(x)
}
