#' Cross-validation Function
#'
#' Performs cross-validation for generalized linear models using the glmnet package.
#'
#' @param x Predictor variables as a data frame or matrix.
#' @param y Response variable as a vector.
#' @param alpha Regularization parameter for elastic net regularization.
#' @param lambda Regularization parameter for penalized models.
#' @param nfolds Number of folds for cross-validation.
#' @param type Type of model, either "default" for regression or "class" for classification.
#'
#' @return A list containing the fitted model, best lambda value, selected features, alpha value, number of folds, and type of model.
#'
#' @export
#'
#' @examples
#' # Load required libraries
#'library(glmnet)

#'# Generate sample data
#'set.seed(123)
#'x <- matrix(rnorm(100), ncol = 5)
#'y <- rnorm(20)

#'# Perform cross-validation
#'cv_result <- crossValidation(x, y, nfolds = 5, type = "default")

#'# Display the results
#'print(cv_result)



crossValidation = function(x, y, alpha = NULL, lambda = NULL, nfolds = 10, type = "default"){
    if (!is.null(alpha) && !is.numeric(alpha)) {
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
    if(is.null(alpha)){
        alpha = sample(seq(0.1, 0.9, by = 0.1), 1)
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
        print("coefficients==========")
        print(coefficients)
        coefficients_sum <- Reduce(`+`, coefficients)
        coefficients_matrix <- as.matrix(coefficients_sum)[-1, , drop=FALSE]
        selected_vars <- which(sapply(coefficients_matrix, function(x) x != 0))
        # selected_indices <- order(coefficients_matrix, decreasing = TRUE)
        selectedFeatures <- names(coefficients_matrix[selected_vars, ])

    }
    print(coef(fit))
    cat("Most Informative Predictors: \n", selectedFeatures)
    return(list(fit = fit, bestLambda = best_lambda, features = selectedFeatures, alpha = alpha, nfolds = nfolds, type = type))
}
