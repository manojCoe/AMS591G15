crossValidation = function(x, y, alpha = 1, lambda = NULL, nfolds = 10, type = "default"){
    if (!is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.null(lambda) && !is.numeric(lambda)) {
        stop("lambda parameter must be a numeric value")
    }
    if(!is.data.frame(x) & is.matrix(x)){
        x = data.frame(x)
    }
    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data

    # Convert target to factor for multinomial classification
    if ( type == "class" & length(unique(y)) > 2) {
        y <- as.factor(y)
    }
    print(class(x))
    print(head(x))

    if(type == "class"){
        if (length(unique(y)) == 2) {
            # Binary classification
            fit <- cv.glmnet(x, y, family = "binomial", alpha = alpha, type.measure = type, nfolds = nfolds)
        } else {
            # Multiclass classification
            fit <- cv.glmnet(x, y, family = "multinomial", alpha = alpha, type.measure = type, nfolds = nfolds)
        }
    }
    else{
        fit <- cv.glmnet(x, y, alpha = alpha, nfolds = nfolds)
    }
    # Get the index of the lambda value with the minimum mean cross-validated error
    best_lambda_index <- which.min(fit$cvm)

    # Retrieve the best lambda value
    best_lambda <- fit$lambda[best_lambda_index]
    print(paste("best lambda: ", best_lambda))

    selectedFeatures = getInformativePredictors(fit)
    cat("Most Informative Predictors: \n", selectedFeatures)
    return(list(fit = fit, bestLambda = best_lambda, features = selectedFeatures, alpha = alpha, nfolds = nfolds, type = type))
}
