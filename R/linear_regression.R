library(glmnet)
#' Linear Regression
#'
#' This function performs linear regression using the glmnet package.
#'
#' @param x A matrix or data frame of predictor variables.
#' @param y A numeric vector of response variable.
#' @param alpha The elastic-net mixing parameter. Default is 1 (ridge regression).
#' @param lambda The value of the penalty parameter lambda. If NULL, a default value of 0.01 is used.
#' @param importance Logical. If TRUE, the function will calculate feature importance using the k important predictors.
#' @param type Character string indicating the type of model. Currently, only "default" is supported.
#' @param nfolds Number of folds for cross-validation. Default is 10.
#' @param ignoreWarnings Logical. If TRUE, warnings will be suppressed. Default is TRUE.
#' @param k The number of important predictors to select if importance is TRUE. Default is 6.
#'
#' @return A list containing the fitted model, coefficients, and selected features.
#'
#' @export
#'
#' @examples
#' x <- matrix(rnorm(100*20), 100, 20)
#' y <- rnorm(100)
#' fit <- linear_regression(x, y)
#' summary(fit$fit)
#' coef(fit$fit)
#' fit$selectedFeatures
linear_regression <- function(x, y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10, ignoreWarnings = T, k = 6) {
    if (!is.null(alpha) && !is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.null(lambda) && !is.numeric(lambda)) {
        stop("lambda parameter must be a numeric value")
    }
    if(!is.character(type)){
        stop("parameter 'type' must be a string.")
    }
    if(type == "class"){
        stop("Linear Regression only supports type = regression")
    }
    if(!is.logical(importance)){
        stop("parameter 'importance' must be of type logical TRUE/FALSE")
    }
    if(!is.numeric(nfolds)){
        stop("parameter 'nfolds' must be of type numeric.")
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

    if(length(colnames(x)) == 1){
        data = data.frame(x, y)
        fit = lm(y ~ x, data = data)
        result = list(fit = fit, coef = coef(fit), selectedFeatures = "x")
        return(result)
    }


    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data


    if(importance){
        # cv.fit = crossValidation(x, y, alpha = alpha, type = type, nfolds = nfolds)
        # print(cv.fit)
        selected_vars = getKImportantPredictors(x, y, type = "default", k = k )
        x = x[, selected_vars]
        # lambda = cv.fit$bestLambda
    }

    if (is.null(lambda)) {
        lambda <- 0.01
        if(!ignoreWarnings){
            warning("Please provide lambda. Setting lambda to default value 0.01")
        }
    }


    fit = glmnet(x, y, alpha = alpha, lambda = lambda)

    result = list(fit = fit, coef = coef(fit), selectedFeatures = colnames(x))
    return(result)
}
