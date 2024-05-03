library(glmnet)
#' Title
#'
#' @param x The predictor variables.
#' @param y The response variable.
#' @param alpha The elastic net mixing parameter.
#' @param lambda The penalty parameter.
#' @param importance Logical indicating whether to perform feature importance selection.
#' @param type The type of logistic regression. Currently only supports "class" for classification.
#' @param nfolds The number of folds for cross-validation.
#' @param ignoreWarnings Logical indicating whether to ignore warnings.
#' @param k The number of important features to select.
#'
#' @return A list containing the fitted model, coefficients, and selected features.
#' @export
#'
#' @examples
#'# Load required libraries
#'library(glmnet)

#'# Generate sample data
#'set.seed(123)
#'x <- matrix(rnorm(100), ncol = 5)
#'y <- sample(0:1, 20, replace = TRUE)

#'# Perform logistic regression
#'logistic_result <- logistic_regression(x, y, alpha = 0.5, lambda = 0.1, importance = TRUE, type = "class", nfolds = 5)

#'# Display the results
#'print(logistic_result)



logistic_regression <- function(x, y, alpha = alpha, lambda = NULL, importance = FALSE, type = "default", nfolds = 10, ignoreWarnings = T, k = 6) {
    if (!is.null(alpha) && !is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.null(lambda) && !is.numeric(lambda)) {
        stop("lambda parameter must be a numeric value")
    }
    if(!is.character(type)){
        stop("parameter 'type' must be a string. One of ('default', 'class')")
    }
    if(type != "class"){
        stop("Logistic regression only supports type = class")
    }
    if(!is.logical(importance)){
        stop("parameter 'importance' must be of type logical TRUE/FALSE")
    }
    if(!is.numeric(nfolds)){
        stop("parameter 'nfolds' must be of type numeric.")
    }
    if(importance && !is.numeric(k)){
        stop("parameter 'k' must be of type numeric")
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
    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data

    # Convert target to factor for multinomial classification
    y = as.factor(y)

    if(importance){
        # cv.fit = crossValidation(x, y, alpha = alpha, type = "class", nfolds = nfolds)
        # print(cv.fit)
        # x = x[, cv.fit$features]
        # lambda = cv.fit$bestLambda
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
