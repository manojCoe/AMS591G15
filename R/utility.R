convertCatToNumeric = function(x){
    categoricalColumns = sapply(x, is.character)

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        x = model.matrix(~ ., data = x)
    }
    return(x)
}

rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}
