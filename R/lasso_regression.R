library(glmnet)

#' Lasso Regression
#'
#' This function performs Lasso Regression using the glmnet package. It allows for both regression and classification tasks.
#'
#' @param x A data frame or matrix containing the predictor variables.
#' @param y A numeric vector or factor for regression and classification tasks respectively.
#' @param alpha A numeric value between 0 and 1. It controls the elastic net mixing parameter. If alpha = 1, the function performs Lasso regression.
#' @param lambda A numeric value for the regularization parameter. If not provided, it defaults to 0.01.
#' @param importance A logical value. If TRUE, the function returns the importance of the predictors.
#' @param type A character string. It can be either 'default' for regression tasks or 'class' for classification tasks.
#' @param nfolds An integer value. It is used for cross-validation in the case of importance = TRUE and k is not provided.
#' @param ignoreWarnings A logical value. If TRUE, warnings are ignored.
#' @param k An integer value. It is used to select the top k most important predictors.
#'
#' @return A list containing the glmnet fit, coefficients, and selected features.
#'
#' @examples
#' # Load the required library
#'library(glmnet)

#'# Generate sample data
#'set.seed(123)
#'x <- matrix(rnorm(100), ncol = 5)
#'y <- rnorm(20)

#'# Fit a Lasso regression model
#'lasso_model <- lasso_regression(x = x, y = y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10, ignoreWarnings = TRUE, k = 3)

#'# Print the selected features
#'print(lasso_model$selectedFeatures)

#'# Print the coefficients
#'print(lasso_model$coef)

#'# Print the fitted model
#'print(lasso_model$fit)

#'# Make predictions if needed
#'#  predictions <- predict(lasso_model$fit, newx = x_test)


lasso_regression <- function(x, y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10, ignoreWarnings = T, k = 6) {
    if (!is.null(alpha) && !is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.null(lambda) && !is.numeric(lambda)) {
        stop("lambda parameter must be a numeric value")
    }
    if(!is.character(type)){
        stop("parameter 'type' must be a string. One of ('default', 'class')")
    }
    if(!is.logical(importance)){
        stop("parameter 'importance' must be of type logical TRUE/FALSE")
    }
    if(!is.numeric(nfolds)){
        stop("parameter 'nfolds' must be of type numeric.")
    }
    if(!is.null(k)){
        if(!is.numeric(k)){
            stop("parameter 'k' must be of type numeric")
        }
        else if ( k < 1){
            stop("parameter 'k' must be >= 1.")
        }
    }
    if(is.matrix(x)){
        x = data.frame(x)
    }
    if(ncol(x)>nrow(x)){
        print("p>>n setting importance = TRUE for selecting k most informative predictors.")
        importance = TRUE
        if(is.null(k)){
            warning("parameter 'k' is missing, setting it to default value : 6")
            k = 6
        }
    }
    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data

    # Convert target to factor for multinomial classification
    if ( type == "class" & length(unique(y)) >= 2) {
        y <- as.factor(y)
    }

    if(is.null(k) && importance){
        warning("Please provide parameter 'k' for selecting k most informative predictors.
                Defaulting to use cross validation to obtain important features.")
        cv.fit = crossValidation(x, y, alpha = alpha, type = type, nfolds = nfolds)
        print(cv.fit)
        x = x[, cv.fit$features]
        lambda = cv.fit$bestLambda
    }
    else{
        selected_vars = getKImportantPredictors(x, y, type = type, k = k )
        x = x[, selected_vars]
    }

    if (is.null(lambda)) {
        lambda <- 0.01
        if(!ignoreWarnings){
            warning("Please provide lambda. Setting lambda to default value 0.01")
        }
    }
    if(type == "class"){
        if (length(unique(y)) == 2) {
            # Binary classification
            fit <- glmnet(x, y, family = "binomial", alpha = alpha, lambda = lambda)
        } else {
            # Multiclass classification
            fit <- glmnet(x, y, family = "multinomial", alpha = alpha, lambda = lambda)
        }
    }
    else{
        fit = glmnet(x, y, alpha = alpha, lambda = lambda)
    }
    result = list(fit = fit, coef = coef(fit), selectedFeatures = colnames(x))
    return(result)
}

