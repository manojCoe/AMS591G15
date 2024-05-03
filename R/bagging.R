 library(glmnet)
 #' Bagging Function
 #'
 #' Implements bagging (bootstrap aggregating) for machine learning models.
 #'
 #' @param x Training features as a data frame, matrix, or numeric vector.
 #' @param y Training labels as a data frame, matrix, factor, or numeric vector.
 #' @param testData Test data as a data frame or matrix.
 #' @param model_type Type of model to use for bagging. Options include "linear", "logistic", "ridge", "lasso", "elastic_net", and "svm".
 #' @param responseVariable Name of the response variable.
 #' @param R Number of bootstrap samples.
 #' @param type Type of model, either "default" for regression or "class" for classification.
 #' @param lambda Regularization parameter for models that support it (e.g., Ridge, Lasso).
 #' @param alpha Regularization parameter for models that support it (e.g., Lasso, Elastic Net).
 #' @param ignoreWarnings Whether to ignore warnings during model fitting.
 #' @param importance Whether to compute variable importance.
 #' @param nfolds Number of folds for cross-validation.
 #' @param kernel Kernel type for SVM models.
 #' @param cost, degree, coef0, gamma, epsilon Hyperparameters for SVM models.
 #' @param k Number of nearest neighbors for kNN models.
 #'
 #' @return A list containing predicted values, variable importance scores, and additional metrics.
 #'
 #' @export
 #'
 #' @examples
 #' bagging(x = train_features, y = train_labels, testData = test_data, model_type = "svm", responseVariable = "class", R = 10, type = "class", nfolds = 5, kernel = "radial", cost = 1)
 #'
bagging <- function(x, y, testData, model_type,
                    responseVariable = NULL, R = 10,
                    type = "default", lambda = NULL,
                    alpha = NULL, ignoreWarnings = T,
                    importance = NULL, nfolds = 10,
                    kernel = "radial",cost = 1,
                    degree = 3, coef0 = 0, gamma = NULL,
                    epsilon = 0.1, k = 6
                    ) {
    if(!is.data.frame(x) && !is.matrix(x) && !is.numeric(x)){
        print(class(x))
        stop("x should be of type data.frame, matrix or numeric")
    }
    if(!is.data.frame(y) && !is.matrix(y) && !is.factor(y) && !is.numeric(y)){
        stop("y should be of type data.frame, matrix, factor or numeric")
    }
    if(is.numeric(y) && type == "class" && !(model_type %in% c("svm", "logistic"))){
        stop("class has 1 or 0 observations; not allowed for regression model")
    }
    if(!is.data.frame(testData) && !is.matrix(testData)){
        if(is.null(testData)){
            stop("Missing attribute 'testData'")
        }
        stop("y should be of type data.frame or matrix")
    }
    if(is.null(responseVariable)){
        stop("parameter 'responseVariable' should be a string.")
    }
    if( !(model_type %in% c("linear", "logistic", "ridge", "lasso", "elastic_net", "svm")) ){
        stop("Please provide a valid model_type: ('linear', 'logistic', 'ridge', 'lasso', 'elastic_net', 'svm)")
    }
    if (!is.null(alpha) && !is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.numeric(R)) {
        stop("parameter 'R' must be a numeric value")
    }
    if (!is.null(lambda) && !is.numeric(lambda)) {
        stop("lambda parameter must be a numeric value")
    }
    if(!is.character(type)){
        stop("parameter 'type' must be a string. One of ('default', 'class')")
    }
    if(!is.logical(ignoreWarnings)){
        stop("parameter 'ignoreWarnings' must be of type logical TRUE/FALSE")
    }
    if(model_type == "elastic_net" && is.null(alpha)){
        alpha = 0.5
        if(!ignoreWarnings){
            warning("Missing alpha parameter. Setting to default value 0.5")
        }
    }
    if(!is.null(kernel) && !(kernel %in% c("linear", "radial", "polynomial", "sigmoid")) ){
        stop("Please provide a valid kernel type: ('linear', 'radial', 'polynomial', 'sigmoid')")
    }
    if(!is.null(epsilon) && !is.numeric(epsilon)){
        stop("epsilon parameter accepts only 'numeric' type values ")
    }
    if(!is.null(cost) && !is.numeric(cost)){
        stop("cost parameter accepts only 'numeric' type values ")
    }
    if(!is.null(coef0) && !is.numeric(coef0)){
        stop("coef0 parameter accepts only 'numeric' type values ")
    }
    if(!is.null(degree) &&!is.numeric(degree)){
        stop("degree parameter accepts only 'numeric' type values ")
    }

    if(!is.data.frame(x) && !is.matrix(x) && is.numeric(x)){
        x = data.frame(x)
        colnames(x) = "x"
    }

    variable_importance = setNames(rep(0, ncol(x)), colnames(x)) # Variable importance score
    data = data.frame(x,y)
    data = na.omit(data)
    n <- nrow(data)
    n_test = nrow(testData)
    selected_vars = colnames(x)
    p <- ncol(data) - 1  # Number of predictor variables
    predicted_values <- matrix(NA, nrow = n_test, ncol = R)  # Matrix to store predicted values from each model
    coefficients = NULL

    if(p>n){
        importance = TRUE
        if(!is.null(lambda)){
            warning("Number of predictors > number of observations.
                    Lambda value will be replaced by lambda.min given by cross-validation"
                    )
        }
    }
    else if(!is.null(lambda)){
        importance = FALSE
        if(importance && !ignoreWarnings){
            warning("Ignoring parameter importance = TRUE as lambda value is given.")
        }
    }
    else if(is.null(importance)){
        importance = FALSE
    }

    if(importance && !is.numeric(k)){
        stop("parameter 'k' must be of type numeric")
    }
    if(!is.null(k) && k<1){
        stop("parameter 'k' must be >= 1.")
    }


    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE)
    print(paste("no:of rows in testData: ", nrow(testSet)))

    if(!identical(colnames(x), colnames(testSet))){
        print(colnames(x))
        print(colnames(testSet))
        stop("x and predictors of testData are not same")
    }

    # if(!is.matrix(y)){
    #     y = as.matrix(y)
    # }
    for (i in 1:R) {
        print(paste("Iterration: ", i))
        # Generate bootstrap sample
        sample_indices <- sample(1:n, replace = TRUE)
        bootstrap_data <- data[sample_indices, ]

        # Train model on bootstrap sample
        if (model_type == "linear") {
            print("Inside Linear")
            # model <- lm(formula = formula, data = bootstrap_data)
            model = linear_regression(x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                                     y = bootstrap_data[, ncol(bootstrap_data)],
                                     alpha = 1,
                                     importance = importance,
                                     type = type,
                                     ignoreWarnings = ignoreWarnings,
                                     nfolds = nfolds, k = k,
                                     lambda = lambda
                                     )
            coefficients = model$coef
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
        } else if (model_type == "logistic") {
            # model <- logisticRegression(x = bootstrap_data[, -ncol(bootstrap_data)], y = bootstrap_data[, ncol(bootstrap_data)], lambda = lambda)
            model = logistic_regression(x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                                     y = bootstrap_data[, ncol(bootstrap_data)],
                                     alpha = 1,
                                     importance = importance,
                                     type = type,
                                     ignoreWarnings = ignoreWarnings,
                                     nfolds = nfolds, k = k,
                                     lambda = lambda
                                    )
            coefficients = model$coef
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
        } else if (model_type == "ridge") {
            # model <- cv.glmnet(as.matrix(bootstrap_data[, -ncol(bootstrap_data)]), bootstrap_data[, ncol(bootstrap_data)], alpha = 0)
            model = ridge_regression(x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                                     y = bootstrap_data[, ncol(bootstrap_data)],
                                     alpha = 0,
                                     importance = importance,
                                     type = type,
                                     ignoreWarnings = ignoreWarnings,
                                     nfolds = nfolds, k = k,
                                     lambda = lambda
                                    )
            coefficients = model$coef
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
        } else if (model_type == "lasso") {
            # model <- cv.glmnet(as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
            #                    bootstrap_data[, ncol(bootstrap_data)], alpha = 1, lambda = lambda)
            model = lasso_regression(x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                                     y = bootstrap_data[, ncol(bootstrap_data)],
                                     importance = importance,
                                     type = type,
                                     alpha = 1,
                                     ignoreWarnings = ignoreWarnings,
                                     nfolds = nfolds, k = k,
                                     lambda = lambda
                                     )
            coefficients = model$coef
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
            # print(variable_importance)
        } else if (model_type == "elastic_net") {
            model = elastic_net_regression(x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                                     y = bootstrap_data[, ncol(bootstrap_data)],
                                     alpha = 0.5,
                                     importance = importance,
                                     type = type,
                                     ignoreWarnings = ignoreWarnings,
                                     nfolds = nfolds, k = k,
                                     lambda = lambda
                                     )
            coefficients = model$coef
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
        } else if(model_type == "svm"){
            x = as.matrix(bootstrap_data[, -ncol(bootstrap_data)])
            y = bootstrap_data[, ncol(bootstrap_data)]
            data = data.frame(x, y)
            # tune.control = tune.control(sampling = "cross", cross = nfolds)

            model = svmModel(data, responseVariable = "y",
                             importance = importance, kernel = kernel,
                             type = type, cost = cost, gamma = NULL,
                             epsilon = epsilon, degree = degree, coef0 = coef0,
                             nfolds = nfolds, k = k
            )
            # model = svmModel(data, responseVariable = "y", importance = TRUE, kernel = "radial", type = "class")
            testSet = scale(testSet)
            selected_vars = model$selectedFeatures
            model = model$fit
            # print(selectedFeatures)
            for (key in names(variable_importance)) {
                if (key %in% selected_vars) {
                    variable_importance[[key]] <- variable_importance[[key]] + 1
                }
            }
        }
        test_x = testSet[, selected_vars]

        if (model_type == "svm"){
            if(type == "class"){
                preds = predict(model, test_x, type = "class")
                # print(head(preds))
                # class_levels <- levels(model$fitted)
                # # Convert numerical labels to character labels using class levels
                # preds <- class_levels[preds]
                predicted_values[, i] = preds
            } else{
                predicted_values[, i] = predict(model, test_x)
            }

        }
        else {
            if(type == "default"){
                if(model_type == "linear"){
                    if(is.numeric(test_x)){
                        test_x = data.frame(test_x)
                        colnames(test_x) = "x"
                    }
                    predicted_values[, i] = predict_regression(coefficients, test_x, model)
                }
                else{
                    predicted_values[, i] = predict_regression(coefficients, test_x)
                }

            }  else{
                predicted_values[, i] = predict(model, test_x, type = "class")
            }
        }
        # Make predictions on original dataset
        # print(tail(predicted_values))
    }
    variable_importance <- variable_importance / R
    if(type != "class"){
        final_predicted_values = rowMeans(predicted_values)
        rmse_score = rmse(final_predicted_values, testData[[responseVariable]])

        if(is.null(coefficients)){
            coefficients = coef(model)
        }
        return(list(predictions = final_predicted_values, variable_importance = variable_importance, rmse = rmse_score, coefficients = coefficients))
    }
    else{
        print("Majority Vote")
        final_predicted_values = apply(predicted_values, 1, function(x) {
            names(which.max(table(x)))
        })
        return(list(predictions = final_predicted_values, variable_importance = variable_importance, results = table(final_predicted_values, testData[[responseVariable]])))
    }

}

