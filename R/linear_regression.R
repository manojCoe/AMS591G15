library(glmnet)

linear_regression <- function(x, y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10, ignoreWarnings = T) {
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

    if(length(colnames(x)) == 1){
        data = data.frame(x, y)
        fit = lm(y ~ x, data = data)
        result = list(fit = fit, coef = coef(fit), selectedFeatures = "x")
        return(result)
    }


    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data


    if(importance){
        cv.fit = crossValidation(x, y, alpha = alpha, type = type, nfolds = nfolds)
        print(cv.fit)
        x = x[, cv.fit$features]
        lambda = cv.fit$bestLambda
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
