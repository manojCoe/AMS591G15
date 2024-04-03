convertCatToNumeric = function(x){
    print("Inside convertCatToNumeric()")
    categoricalColumns = sapply(x, is.character)

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        print("X contains categorical data.")
        x = model.matrix(~ ., data = x)
    }
    print("executed convertCatToNumeric()")
    return(x)
}

rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}
