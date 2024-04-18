library(glmnet)

svm_classification_glmnet <- function(x, y, cv = FALSE, useRegularization = FALSE, alpha, lambda, lambdaList = list()) {
    if(class(cv) != "logical"){
        stop("cv (Cross Validation) parameter accepts only 'logical' type values ")
    }
    if(class(useRegularization) != "logical"){
        stop("useRegularization parameter accepts only 'logical' type values ")
    }
    if(class(lambda) != "numeric"){
        stop("lambda parameter accepts only 'numeric' type values ")
    }
    if(class(alpha) != "numeric"){
        stop("alpha parameter accepts only 'numeric' type values ")
    }
    if(class(lambdaList) != "list"){
        stop("lambdaList parameter accepts only 'list' type values ")
    }
    x = convertCatToNumeric(x, intercept = FALSE)
    x = x$data

    # Convert target to factor for multinomial classification
    if (length(unique(y)) > 2) {
        y <- as.factor(y)
        # y_test <- as.factor(y_test)
    }
    if(cv){
        if(is.null(alpha)){
            alpha = 1
            warning("Please provide alpha. Setting alpha to 1 for model fitting")
        }
        if(length(lambdaList) == 0){
            if (length(unique(y)) == 2) {
                # Binary classification
                fit <- cv.glmnet(x, y, family = "binomial", alpha = alpha)
            } else {
                # Multiclass classification
                fit <- cv.glmnet(x, y, family = "multinomial", alpha = alpha)
            }
        }
        else if(length(lambdaList) > 0){
            if (length(unique(y)) == 2) {
                # Binary classification
                fit <- cv.glmnet(x, y, family = "binomial", alpha = alpha, lambda = lambda)
            } else {
                # Multiclass classification
                fit <- cv.glmnet(x, y, family = "multinomial", alpha = alpha, lambda = lambda)
            }
        }
    }
    else{
        if(is.null(lambda)){
            lambda = 0.01
            warning("Please provide lambda. Setting lambda to default value 0.01")
        }
        if(useRegularization){

            if(is.null(alpha)){
                alpha = 1
                warning("Please provide alpha. Setting alpha to 1 for model fitting")
            }

            if (length(unique(y)) == 2) {
                # Binary classification
                fit <- glmnet(x, y, family = "binomial", alpha = alpha, lambda = lambda)
            } else {
                # Multiclass classification
                fit <- glmnet(x, y, family = "multinomial", alpha = alpha, lambda = lambda)
            }
        }
        else{
            if (length(unique(y)) == 2) {
                # Binary classification
                fit <- glmnet(x, y, family = "binomial", lambda = lambda)
            } else {
                # Multiclass classification
                fit <- glmnet(x, y, family = "multinomial", lambda = lambda)
            }
        }

    }

    return(fit)
}

