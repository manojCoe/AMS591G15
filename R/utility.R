library(randomForest)

#' Get K Important Predictors
#'
#' Retrieves the top K important predictors using Random Forest feature importance.
#'
#' @param x The predictor variables.
#' @param y The response variable.
#' @param type The type of analysis ("default" or "class").
#' @param k The number of important predictors to select.
#'
#' @return A character vector of the top K important predictors.
#' @export
#'
#' @examples
#' # Load required library
#'library(randomForest)

#'# Generate sample data
#'set.seed(123)
#'x <- matrix(rnorm(100), ncol = 5)
#'y <- sample(0:1, 20, replace = TRUE)

#'# Get top 3 important predictors
#'important_predictors <- getKImportantPredictors(x, y, type = "default", k = 3)

#'# Display the top important predictors
#'print(important_predictors)

getKImportantPredictors = function(x, y, type = "default", k = 6){
    if(is.data.frame(y)){
        y = as.vector(y$y)
    }
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

getAccuracyForClassification = function(preds, act){
    res = mean(preds == act)
    return(res)
}

getWeightedAverage = function(accuracies, all_predictions, act){
    actual_labels = levels(act)
    # if (length(unique(actual_labels)) == 2) {
    #     weights <- 1 / accuracies
    # } else {
    #     exp_accuracies <- exp(accuracies)
    #     weights <- exp_accuracies / sum(exp_accuracies)
    # }

    # Rank models based on accuracy
    rank <- rank(-accuracies)  # Use negative sign for descending order

    # Calculate weights based on rank
    weights <- 1 / rank
    weights <- weights / sum(weights)

    # Calculate weighted average
    final_predictions <- apply(all_predictions, 1, function(row) sum(row == unique(actual_labels)) / length(unique(actual_labels)))

    # Convert to final class labels
    if (length(unique(actual_labels)) == 2) {
        final_predictions <- ifelse(final_predictions >= 0.5, unique(actual_labels)[1], unique(actual_labels)[2])
    } else {
        final_predictions <- sapply(1:length(final_predictions), function(i) {
            class_probs <- table(all_predictions[i, ])
            names(class_probs)[which.max(class_probs)]
        })
    }
    final_predictions = as.character(final_predictions)
    return(final_predictions)

}
