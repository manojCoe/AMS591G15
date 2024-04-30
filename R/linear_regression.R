library(glmnet)

linear_regression <- function(x, y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10) {
    if (!is.numeric(alpha)) {
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
    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data

    if(importance){
        cv.fit = crossValidation(x, y, alpha = alpha, type = type, nfolds = nfolds)
        print(cv.fit)
        x = x[, cv.fit$features]
        lambda = cv.fit$bestLambda
        # fit = glmnet(x[, cv.fit$features], y, alpha = alpha, lambda = cv.fit$bestLambda)
        # result = list(fit = fit, coef = coef(fit), features = cv.fit$features)
        # return(result)
    }

    if (is.null(lambda)) {
        lambda <- 0.01
        warning("Please provide lambda. Setting lambda to default value 0.01")
    }


    fit = glmnet(x, y, alpha = alpha, lambda = lambda)

    result = list(fit = fit, coef = coef(fit), selectedFeatures = colnames(x))
    return(result)
}
