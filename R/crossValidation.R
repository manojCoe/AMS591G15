library(glmnet)

perform_ridge_regression <- function(x, y, data, response_var, predictor_vars, alpha = 0, modelType = "linear", nfolds = 10) {

    if(modelType == "linear"){
        # Fit the ridge regression model
        model <- glmnet(x, y, alpha = alpha)

        # View summary of the model
        summary(model)

        # Perform k-fold cross-validation to find optimal lambda value
        cvModel <- cv.glmnet(x, y, alpha = alpha)

        # Find the optimal lambda value that minimizes test MSE
        best_lambda <- cvModel$lambda.min
        model <- glmnet(x = X, y = y, alpha = alpha, lambda = best_lambda)
        return(list(model = model, best_lambda = best_lambda))
    }
    else if(modelType == "logistic"){
        num_classes = length(unique(as.factor(y)))
        family = "binomial"
        if(num_classes > 2){
            family = "multinomial"
        }
        # Fit the ridge regression model
        model <- glmnet(x, y, alpha = alpha, family = family)

        # View summary of the model
        summary(model)

        # Perform k-fold cross-validation to find optimal lambda value
        cvModel <- cv.glmnet(x, y, alpha = alpha, family = family, nfolds = nfolds)
        best_lambda <- cvModel$lambda.min
        model <- glmnet(x = X, y = y, alpha = alpha, lambda = best_lambda, family = family)
        return(list(model = model, best_lambda = best_lambda))

    }
    else if(modelType == "svm"){
        cvModel = tune.svm(x = X, y = y, kernel = "linear", tunecontrol = tune.control(nfold = nfolds))
        best_cost <- cvModel$best.parameters$cost
        model <- svm(x = X, y = y, kernel = "linear", cost = best_cost)
        return(list(model = model, best_cost = best_cost))
    }

}
