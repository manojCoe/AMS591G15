library(randomForest)

getKImportantPredictors = function(x, y, type = "default", k = 6){
    # Train the Random Forest model
    rf_model <- randomForest(x = x, y = y, ntree = 500, importance = TRUE)

    # Get feature importance
    if(type == "class"){
        importanceScores <- rf_model$importance[, "MeanDecreaseAccuracy"]
    }
    else{
        importanceScores <- rf_model$importance[, "%IncMSE"]
    }

    # Select the top features
    # top_features <- rownames(importance_scores)[order(importance_scores, decreasing = TRUE)][1:5]
    topFeatures <- names(importanceScores)[order(importanceScores, decreasing = TRUE)][1:k]
    topFeatures = topFeatures[!is.na(topFeatures)]
    return(topFeatures)
}

convertCatToNumeric = function(x, intercept = TRUE, toDataFrame = FALSE){
    # print("Inside convertCatToNumeric()")
    categoricalColumns = sapply(x, is.character)
    hasCategorical = FALSE

    # If there are any categorical columns, convert them to dummy variables
    if (any(categoricalColumns)) {
        # print("X contains categorical data.")
        hasCategorical = TRUE
        modelMatrix = model.matrix(~ ., data = x)
        if(intercept == FALSE){
            x = modelMatrix[, -1]
        }
        else{
            x = modelMatrix
        }
    }
    if(!toDataFrame && is.data.frame(x)){
        x = as.matrix(x)
    }
    else if(toDataFrame && !is.data.frame(x)){
        x = data.frame(x)
    }
    # print("executed convertCatToNumeric()")
    return(list(data = x, hasCategorical = hasCategorical))
}

rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}

softThreshold = function(x, lambda) {
    result = sign(x) * max(abs(x) - lambda, 0)
}

preprocessTestData = function(testSet, intercept = TRUE, features = NULL){
    if(!is.null(features)){
        if(class(features) != "character"){
            stop("parameter 'features' must be of type character vector")
        }
        if(length(features) == 0){
            stop("length of 'features' is 0, expected > 0")
        }
        else{
            x = convertCatToNumeric(testSet[, features], intercept)
        }
    }
    else{
        x = convertCatToNumeric(testSet, intercept)
    }

    hasCategorical = x$hasCategorical
    x = x$data

    if(!hasCategorical & intercept){
        print("Adding intercept column to matrix.")
        x = convertCatToNumeric(testSet, intercept)
    }
    if(!is.matrix(x)){
        x = as.matrix(x)
    }
    return(x)
}

getInformativePredictors = function(model){
    coefficients = as.matrix(coef(model))[-1, ]
    selectedPredictors = names(which(coefficients != 0))
    return(selectedPredictors)
}
