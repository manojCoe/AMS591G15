#'
#' Ensemble Model
#'
#' Fits an ensemble model using a combination of different base models.
#'
#' @param x Predictor variables as a data frame or matrix.
#' @param y Response variable as a vector.
#' @param testData Test data for prediction.
#' @param responseVariable Name of the response variable in the testData.
#' @param models List of base models to be included in the ensemble.
#' @param alpha Regularization parameter for certain models.
#' @param lambda Regularization parameter for certain models.
#' @param bagging Logical indicating whether to use bagging for model combination.
#' @param R Number of bootstrap replicates if bagging is used.
#' @param importance Logical indicating whether to consider feature importance in model selection.
#' @param type Type of model, either "default" for regression or "class" for classification.
#' @param nfolds Number of folds for cross-validation.
#' @param ignoreWarnings Logical indicating whether to ignore warnings.
#' @param kernel Kernel type for SVM models.
#' @param cost Cost parameter for SVM models.
#' @param degree Degree of polynomial kernel for SVM models.
#' @param coef0 Intercept in polynomial kernel for SVM models.
#' @param gamma Kernel coefficient for "radial", "polynomial", and "sigmoid" kernels for SVM models.
#' @param epsilon Tolerance parameter for SVM models.
#' @param k Number of important predictors to select.
#'
#' @return A list containing the ensemble predictions and, if applicable, evaluation metrics.
#'
#' @export
#'
#' @examples
#' # ENSEMBLE

#'# Binomial
#'x <- matrix(rnorm(10000), ncol = 10)
#'y <- factor(sample(0:1, 1000, replace = TRUE))
#'data = data.frame(x, y)

#'trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
#'trainData <- data[trainID, ]
#'testData <- data[-trainID, ]
#'# model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = F, lambda = 0.1)
#'model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = T, k = 6)
#'model_bagged

#'# Multinomial Classification

#'x <- matrix(rnorm(1500), ncol = 15)
#'y <- factor(sample(1:3, 100, replace = TRUE))

#'data = data.frame(x, y)

#'trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
#'trainData <- data[trainID, ]
#'testData <- data[-trainID, ]
#'model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("logistic","svm","ridge","elastic_net"), responseVariable = "y", R = 15, type = "class", importance = T, k = 6)
#'model_bagged

#'# Regression
#'x <- matrix(rnorm(1000), ncol = 10)
#'y <- rnorm(100)
#'data = data.frame(x, y)

#'trainID <- sample(1:nrow(data), round(0.75 * nrow(data)))
#'trainData <- data[trainID, ]
#'testData <- data[-trainID, ]
#'model_bagged <- ensemble(subset(trainData, select = -y), trainData$y, testData = testData, models = c("svm","ridge","elastic_net"), responseVariable = "y", R = 15, importance = F)
#'model_bagged

#'model = lm(y ~ ., data = trainData)
#'# preds = coef(model) %*% as.matrix(subset(testData, select = -y))
#'test_x = subset(testData, select = -y)
#'preds1 = predict(model, test_x)

#'rmse(preds1, testData$y)




ensemble <- function(x, y, testData, responseVariable = NULL, models,
                     alpha = NULL, lambda = NULL, bagging = FALSE, R = 10,
                     importance = FALSE, type = "default", nfolds = 10,
                     ignoreWarnings = T, kernel = "radial",cost = 1,
                     degree = 3, coef0 = 0, gamma = NULL,epsilon = 0.1, k = 6
                     ){
    if(!is.data.frame(x) && !is.matrix(x)){
        print(class(x))
        stop("x should be of type data.frame or matrix")
    }
    if(!is.data.frame(y) && !is.matrix(y) && !is.factor(y) && !is.numeric(y)){
        stop("y should be of type data.frame, matrix, factor or numeric")
    }
    # if(is.numeric(y) && type == "class"){
    #     stop("class has 1 or 0 observations; not allowed for regression model")
    # }
    if(!is.data.frame(testData) && !is.matrix(testData)){
        if(is.null(testData)){
            stop("Missing attribute 'testData'")
        }
        stop("y should be of type data.frame or matrix")
    }
    if(is.null(responseVariable)){
        stop("parameter 'responseVariable' should be a string.")
    }
    # if( !(model_type %in% c("linear", "logistic", "ridge", "lasso", "elastic_net", "svm")) ){
    #     stop("Please provide a valid model_type: ('linear', 'logistic', 'ridge', 'lasso', 'elastic_net', 'svm')")
    # }
    if (!is.null(alpha) && !is.numeric(alpha)) {
        stop("alpha parameter must be a numeric value")
    }
    if (!is.null(R) && !is.numeric(R)) {
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
    if(!is.null(k)){
        if(!is.numeric(k)){
            stop("parameter 'k' must be of type numeric")
        }
        else if ( k < 1){
            stop("parameter 'k' must be >= 1.")
        }


    }
    # if(model_type == "elastic_net" && is.null(alpha)){
    #     alpha = 0.5
    #     if(!ignoreWarnings){
    #         warning("Missing alpha parameter. Setting to default value 0.5")
    #     }
    # }

    prediction_bagging <- list()
    if (bagging == TRUE){
        for (i in models){
            baggingResult = bagging(x, y, testData = testData,
                                    responseVariable = responseVariable,
                                    model_type = i, R = R, type = type,
                                    importance = importance, ignoreWarnings = ignoreWarnings,
                                    nfolds = nfolds, lambda = lambda, alpha = alpha,
                                    kernel = kernel,cost = cost,
                                    degree = degree, coef0 = coef0, gamma = gamma,
                                    epsilon = epsilon, k = k
                                    )
            predictions <- baggingResult$predictions
            prediction_bagging[[length(prediction_bagging) + 1]] <- predictions
        }
        # return(prediction_bagging)

    } else {
        for (i in models){
            if (i == "linear") {
                model <- linear_regression(x, y, alpha = 1, lambda = lambda, importance = importance, nfolds = nfolds, ignoreWarnings = ignoreWarnings, k = k)
                testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = TRUE)

                print(model$selectedFeatures)

                test_x = as.matrix(testSet[, model$selectedFeatures])

                preds = predict_regression(model$coef, test_x)
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds
            } else if (i == "logistic") {

                model <- logistic_regression(x, y, alpha = 1, lambda = lambda, importance = importance, type = type, nfolds = nfolds, ignoreWarnings = ignoreWarnings, k = k)
                selected_vars = model$selectedFeatures
                testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE, features = selected_vars)
                preds <- predict(model$fit, testSet, type = "class")
                print("LOGISTIC")
                print(table(preds, testData[[responseVariable]]))
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds

            } else if (i == "ridge") {

                model <- ridge_regression(x, y, alpha = 0, lambda = lambda, importance = importance, type = type, nfolds = nfolds, ignoreWarnings = ignoreWarnings, k = k)
                selected_vars = model$selectedFeatures
                if(type == "class"){
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE, features = selected_vars)
                    preds <- predict(model$fit, testSet, type = "class")
                    print("RIDGE")
                    print(table(preds, testData[[responseVariable]]))
                } else{
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = F, features = selected_vars)
                    # preds = predict_regression(model$coef, testSet)
                    preds = predict(model$fit, testSet)
                }
                # preds = predict_regression(model$coef, testSet)
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds

            } else if (i == "lasso") {

                model <- lasso_regression(x, y, alpha = 1, lambda = lambda, importance = importance, type = type, nfolds = nfolds, ignoreWarnings = ignoreWarnings, k = k)
                selected_vars = model$selectedFeatures
                if(type == "class"){
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE, features = selected_vars)
                    preds <- predict(model$fit, testSet, type = "class")
                    print("LASSO")
                    print(table(preds, testData[[responseVariable]]))
                } else{
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = F, features = selected_vars)
                    # preds = predict_regression(model$coef, testSet)
                    preds = predict(model$fit, testSet)
                }
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds

            } else if (i == "elastic_net") {
                if (is.null(alpha) && !importance) {
                    alpha <- 0.5
                    if(!ignoreWarnings){
                        warning("Please provide alpha for elastic_net regression. Setting alpha to default value 0.5")
                    }
                }
                model <- elastic_net_regression(x, y, alpha = alpha, lambda = lambda, importance = importance, type = type, nfolds = nfolds, ignoreWarnings = ignoreWarnings, k = k)
                selected_vars = model$selectedFeatures
                if(type == "class"){
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE, features = selected_vars)
                    preds <- predict(model$fit, testSet, type = "class")
                    # preds = predict(model$fit, testSet)
                    print("ELASTICNET")
                    print(table(preds, testData[[responseVariable]]))
                } else{
                    testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = F, features = selected_vars)
                    # preds = predict_regression(model$coef, testSet)
                    preds = predict(model$fit, testSet)
                }
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds
            }
            else if(i == "svm"){
                data = data.frame(x, y)
                tune.control = tune.control(sampling = "cross", cross = nfolds)

                model = svmModel(data, responseVariable = "y",
                                 importance = importance, kernel = kernel,
                                 type = type, cost = cost, gamma = NULL,
                                 epsilon = epsilon, degree = degree, coef0 = coef0, nfolds = nfolds
                                 )
                # model = svmModel(data, responseVariable = "y", importance = TRUE, kernel = "radial", type = "class")

                testSet = preprocessTestData(subset(testData, select = -which(names(testData) == responseVariable)), intercept = FALSE)
                testSet = scale(testSet)
                test_x = testSet[, model$selectedFeatures]
                print("SVM")
                if(type == "class"){
                    preds <- as.character(predict(model$fit, test_x, type = "class"))
                } else{
                    preds = as.vector(predict(model$fit, test_x))
                }
                prediction_bagging[[length(prediction_bagging) + 1]] <- preds
            }


        }
    }
    if(type != "class"){
        prediction_matrices <- lapply(prediction_bagging, as.matrix)
        # Step 2: Check dimensions of each matrix
        dimensions <- sapply(prediction_matrices, dim)
        print(dimensions)

        combined_matrix <- do.call(rbind, prediction_matrices)
        combined_col_matrix <- do.call(cbind, prediction_bagging)
        mses = apply(combined_col_matrix, 2, function(x){
            rmse(x, testData[[responseVariable]])
        })

        weights <- 1 / mses
        weights <- weights / sum(weights)

        # Calculate weighted average
        weightedPredictions <- rowSums(combined_col_matrix * weights)

        # Step 5: Calculate row means
        final_predicted_values <- rowMeans(combined_matrix)
        rmse_score = rmse(final_predicted_values, testData[[responseVariable]])

        # if(is.null(coefficients)){
        #     coefficients = coef(model)
        # }
        return(list(predictions = final_predicted_values, weightedPredictions = weightedPredictions, modelMses = mses, rmse = rmse_score))
    }
    else{
        print("Majority Vote")
        combined_matrix <- do.call(cbind, prediction_bagging)
        modelAccuracies = apply(combined_matrix, 2, function(x){
            getAccuracyForClassification(x, testData[[responseVariable]])
        })
        weightedPredictions = getWeightedAverage(modelAccuracies, combined_matrix, testData[[responseVariable]])
        if(!is.numeric(combined_matrix)){

            # for (i in 1:ncol(combined_matrix)) {
            #     combined_matrix[, i] <- factor(combined_matrix[, i])
            # }

            # Apply the function to get the final predictions
            final_predicted_values <- apply(combined_matrix, 1, function(x) {
                as.character(names(which.max(table(x))))
            })
            # class_levels <- levels(y)
            # # Convert numerical labels to character labels using class levels
            # final_predicted_values <- class_levels[final_predicted_values]
        }
        else{
            if(length(models) >=2){
                combined_matrix <- matrix(as.numeric(unlist(combined_matrix)), nrow = nrow(prediction_bagging[[1]]))
            }

            # Apply the function to get the final predictions
            final_predicted_values <- apply(combined_matrix, 1, function(x) {
                names(which.max(table(x)))
            })
        }
        final_predicted_values = as.character(final_predicted_values)
        # Convert matrix elements from character to numeric

        return(list(predictions = final_predicted_values,
                    accuracy = as.numeric(modelAccuracies),
                    results = table(final_predicted_values,testData[[responseVariable]]),
                    weightedPredictions = weightedPredictions
                    ))
    }

}
