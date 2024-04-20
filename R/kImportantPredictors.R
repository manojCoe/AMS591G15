importantPredictors = function(x, y, k){
    x = scale(x)
    data = data.frame(x, y)
    corData = cor(data)
    corWithTarget <- abs(corData[, "y"])

    # Rank predictors based on correlation with target variable
    ranked_predictors <- names(corWithTarget)[order(corWithTarget, decreasing = TRUE)]
    print(class(ranked_predictors))

    cat("ranked_predictors: \n", ranked_predictors)

    top_k_predictors <- ranked_predictors[!ranked_predictors %in% "y"][1:k]
    result = as.list(top_k_predictors)
    return(result)
}
