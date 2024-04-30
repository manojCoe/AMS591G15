library(glmnet)

lasso_regression <- function(x, y, alpha = 1, lambda = NULL, importance = FALSE, type = "default", nfolds = 10) {
    if (!is.numeric(alpha)) {
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
    if(is.matrix(x)){
        x = data.frame(x)
    }
    x <- convertCatToNumeric(x, intercept = FALSE)
    x <- x$data

    # Convert target to factor for multinomial classification
    if ( type == "class" & length(unique(y)) >= 2) {
        y <- as.factor(y)
    }

    if(importance){
        cv.fit = crossValidation(x, y, alpha = alpha, type = type, nfolds = nfolds)
        print(cv.fit)
        x = x[, cv.fit$features]
        lambda = cv.fit$bestLambda
    }

    if (is.null(lambda)) {
        lambda <- 0.01
        warning("Please provide lambda. Setting lambda to default value 0.01")
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

