library(glmnet)

bagging <- function(x, y, formula, model_type, R, lambda = NULL, alpha = NULL, bagging_type = "average") {
    data = data.frame(x,y)
    n <- nrow(data)
    p <- ncol(data) - 1  # Number of predictor variables
    predicted_values <- matrix(NA, nrow = n, ncol = R)  # Matrix to store predicted values from each model
    variable_importance <- rep(0, p)  # Variable importance score

    for (i in 1:R) {
        # Generate bootstrap sample
        sample_indices <- sample(1:n, n, replace = TRUE)
        bootstrap_data <- data[sample_indices, ]
        print(head(data))
        x_bootstrap = x[sample_indices, ]
        print(head(x_bootstrap))
        y_bootstrap = y[sample_indices, ]
        print(head(y_bootstrap))

        # Train model on bootstrap sample
        if (model_type == "linear") {
            model <- lm(formula, data = bootstrap_data)
        } else if (model_type == "logistic") {
            model <- logisticRegression(x = data.frame(x_bootstrap), y = data.frame(y_bootstrap))
        } else if (model_type == "ridge") {
            model <- glmnet(x = model.matrix(formula, data = bootstrap_data)[, -1],
                            y = bootstrap_data[, 1], alpha = 0, lambda = lambda)
        } else if (model_type == "lasso") {
            model <- glmnet(x = model.matrix(formula, data = bootstrap_data)[, -1],
                            y = bootstrap_data[, 1], alpha = 1, lambda = lambda)
        } else if (model_type == "elastic_net") {
            model <- glmnet(x = model.matrix(formula, data = bootstrap_data)[, -1],
                            y = bootstrap_data[, 1], alpha = alpha, lambda = lambda)
        }

        # Make predictions on original dataset
        predicted_values[, i] <- predict(model, newx = model.matrix(formula, data = data)[, -1])

        # Update variable importance score
        if (model_type != "linear" && model_type != "logistic") {
            selected_vars <- which(coef(model) != 0)
            variable_importance[selected_vars] <- variable_importance[selected_vars] + 1
        } else if (model_type == "linear" || model_type == "logistic") {
            coef_abs <- abs(coef(model)[-1])  # Exclude intercept
            variable_importance[order(coef_abs, decreasing = TRUE)] <- variable_importance[order(coef_abs, decreasing = TRUE)] + 1
        } else {
            variable_importance <- NA  # Not available for linear regression and logistic regression
        }
    }

    # Average predicted values or perform majority vote
    final_predicted_values <- ifelse(bagging_type == "average", rowMeans(predicted_values), apply(predicted_values, 1, function(x) {
        names(which.max(table(x)))
    }))

    # Normalize variable importance score
    variable_importance <- variable_importance / R

    return(list(predictions = final_predicted_values, variable_importance = variable_importance))
}
