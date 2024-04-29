library(glmnet)

bagging <- function(x, y, formula, model_type, R, type = "response", lambda = NULL, alpha = NULL, bagging_type = "average") {
    data = data.frame(x,y)
    n <- nrow(data)
    p <- ncol(data) - 1  # Number of predictor variables
    predicted_values <- matrix(NA, nrow = n, ncol = R)  # Matrix to store predicted values from each model
    variable_importance <- rep(0, p)  # Variable importance score
    coefficients = NULL
    # if(!is.matrix(y)){
    #     y = as.matrix(y)
    # }
    for (i in 1:R) {
        # print(paste("Iterration: ", i))
        # Generate bootstrap sample
        sample_indices <- sample(1:n, replace = TRUE)
        bootstrap_data <- data[sample_indices, ]
        #print(paste("Class: ", class(bootstrap_data)))

        # x_train <- as.data.frame(bootstrap_data[, -length(bootstrap_data)])
        # y_train <- bootstrap_data[, length(bootstrap_data)]

        # Train model on bootstrap sample
        if (model_type == "linear") {
            print("Inside Linear")
            model <- lm(formula = formula, data = bootstrap_data)
        } else if (model_type == "logistic") {
            model <- logisticRegression(x = bootstrap_data[, -ncol(bootstrap_data)], y = bootstrap_data[, ncol(bootstrap_data)], lambda = lambda)
        } else if (model_type == "ridge") {
            model <- cv.glmnet(as.matrix(bootstrap_data[, -ncol(bootstrap_data)]), bootstrap_data[, ncol(bootstrap_data)], alpha = 0)
        } else if (model_type == "lasso") {
            model <- cv.glmnet(as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                               bootstrap_data[, ncol(bootstrap_data)], alpha = 1, lambda = lambda)
        } else if (model_type == "elastic_net") {
            model <- cv.glmnet(as.matrix(bootstrap_data[, -ncol(bootstrap_data)]),
                               bootstrap_data[, ncol(bootstrap_data)], alpha = 0.5)
        }
        if(type == "response"){
            if(model_type == "linear"){
                predicted_values[, i] = predict_regression(coef(model), data[, -length(data)])
            }
            else{
                predicted_values[, i] = predict_regression(coef(model, s = model$lambda.min), data[, -length(data)])
            }

        }
        else{
            predicted_values[, i] <- predict(model, as.data.frame(data[, -length(data)]))
        }
        # Make predictions on original dataset
        # print(tail(predicted_values))
    }

        # #( ME COMMENTING)
        # # Update variable importance score
        if (model_type != "linear" && model_type != "logistic") {
            coefficients = coef(model)
            if(!is.matrix(coefficients)){
                coefficients = as.matrix(coefficients)
                coefficients_matrix_without_intercept = coefficients[-1, ]
            }
            selected_vars <- which(coefficients_matrix_without_intercept != 0)
            variable_importance[selected_vars] <- variable_importance[selected_vars] + 1
        } else if (model_type == "linear") {
            coef_abs <- abs(coef(model)[-1])  # Exclude intercept
            variable_importance[order(coef_abs, decreasing = TRUE)] <- variable_importance[order(coef_abs, decreasing = TRUE)] + 1
        } else if (model_type == "logistic"){
            coefficients <- coef(model)

            # Sum the coefficients across all classes
            coefficients_sum <- Reduce(`+`, coefficients)
            coefficients_matrix <- as.matrix(coefficients_sum)[-1, , drop=FALSE]

            # Extract the absolute values of coefficients
            coefficients_matrix_without_intercept <- coefficients_matrix[-1, , drop = FALSE]

            selected_vars <- which(coefficients_matrix_without_intercept != 0)

            # Increment count of important predictors for each iteration
            variable_importance[selected_vars] <- variable_importance[selected_vars] + 1
            #(*

            # selected_vars =
            #
            # # Calculate variable importance as the sum of absolute coefficients for each row
            # rowSumsWithoutIntercept <- rowSums(abs(coefficients_matrix_without_intercept))
            #
            # # Sort variable importance in descending order
            # sorted_indices <- order(rowSumsWithoutIntercept, decreasing = TRUE)
            #
            # variable_importance = rowSumsWithoutIntercept[sorted_indices]

            #*)
        }
        else {
            variable_importance <- NA  # Not available for linear regression and logistic regression
        }
        # #( ME COMMENTING)
    # return(rowMeans(predicted_values))
    # }

    # #(* ME COMMENTING)
    print(dim(predicted_values))
    # # Average predicted values or perform majority vote
    if(bagging_type == "average"){
        final_predicted_values = rowMeans(predicted_values)
        rmse_score = rmse(final_predicted_values, y)
        variable_importance <- variable_importance / R
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
        variable_importance <- variable_importance / R
        return(list(predictions = final_predicted_values, variable_importance = variable_importance, results = table(y, final_predicted_values), coefficients = coefficients))
    }
    #
    # print(paste("variable importance count: ", variable_importance))
    # # Normalize variable importance score

    #
    # # print(paste("variable importance: ", variable_importance))
    # #(* ME COMMENTING)

#    return(rowMeans(predicted_values))

}

