library(glmnet)

#' Elastic Net Regression
#'
#' Fits an elastic net regression model using the glmnet package.
#'
#' @param x Predictor variables as a data frame or matrix.
#' @param y Response variable as a vector.
#' @param alpha Regularization parameter controlling the balance between L1 and L2 penalties.
#' @param lambda Regularization parameter controlling the strength of the penalty term.
#' @param importance Logical indicating whether to perform feature selection based on importance.
#' @param type Type of model, either "default" for regression or "class" for classification.
#' @param nfolds Number of folds for cross-validation.
#' @param ignoreWarnings Logical indicating whether to ignore warnings.
#' @param k Number of important predictors to select.
#'
#' @return A list containing the fitted model, coefficients, and selected features.
#'
#' @export
#'
#' @examples
#'library(glmnet)

#'# Example data
#'set.seed(123)
#'train_features <- matrix(rnorm(1000), ncol = 10)
#'train_labels <- rnorm(100)

#'# Fit an elastic net regression model
#'elastic_net_result <- elastic_net_regression(x = train_features, y = train_labels, alpha = 0.5, lambda = 0.1, importance = TRUE, type = "default")

#'# View the results
#'print(elastic_net_result)

elastic_net_regression <- function(x, y, alpha = 0.5,
                                   lambda = NULL, importance = FALSE,
                                   type = "default", nfolds = 5,
                                   ignoreWarnings = T, k = 6
                                   ) {
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

    # if(importance){
    #     cv.fit = crossValidation(x, y, type = type, nfolds = nfolds)
    #     print(cv.fit)
    #     x = x[, cv.fit$features]
    #     lambda = cv.fit$bestLambda
    #     alpha = cv.fit$alpha
    # }

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
    if (is.null(alpha)) {
        alpha <- 0.5
        if(!ignoreWarnings){
            warning("Please provide alpha for elastic_net regression. Setting alpha to default value 0.5")
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
