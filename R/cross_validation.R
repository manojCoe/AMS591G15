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
    if ( type == "class" & length(unique(y)) >= 2) {
        y <- as.factor(y)
    }
    # print(class(x))
    # print(head(x))


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

    # Retrieve the best lambda value
    best_lambda <- fit$lambda.min
    print(paste("best lambda: ", best_lambda))
    if((type != "class") || (type == "class" && length(unique(y))<=2)){
        selectedFeatures = getInformativePredictors(fit)
    }
    else{
        coefficients = as.matrix(coef(fit, s = "lambda.min"))
        coefficients_sum <- Reduce(`+`, coefficients)
        coefficients_matrix <- as.matrix(coefficients_sum)[-1, , drop=FALSE]
        selected_vars <- which(sapply(coefficients_matrix, function(x) x != 0))
        # selected_indices <- order(coefficients_matrix, decreasing = TRUE)
        selectedFeatures <- names(coefficients_matrix[selected_vars, ])

    }
    cat("Most Informative Predictors: \n", selectedFeatures)
    return(list(fit = fit, bestLambda = best_lambda, features = selectedFeatures, alpha = alpha, nfolds = nfolds, type = type))
}
