library(e1071)

svmModel <- function(x, y, cv = FALSE, kernel = "radial", cost = 1, gamma = NULL, cross = 5, epsilon = 0.1) {
    if (cv && is.null(gamma)) {
        stop("gamma parameter must be provided for cross-validation")
    }
    if(class(cv) != "logical"){
        stop("cv (Cross Validation) parameter accepts only 'logical' type values ")
    }
    if( !(kernel %in% c("linear", "radial", "polynomial", "sigmoid")) ){
        stop("Please provide a valid kernel type: ('linear', 'radial', 'polynomial', 'sigmoid')")
    }
    if(class(epsilon) != "numeric"){
        stop("epsilon parameter accepts only 'numeric' type values ")
    }
    if(class(cost) != "numeric"){
        stop("cost parameter accepts only 'numeric' type values ")
    }

    x = convertCatToNumeric(data.frame(x), intercept = FALSE)
    x = x$data

    # Convert target to factor for classification
    y <- as.factor(y)

    if (cv) {
        tune.control <- tune.control(sampling = "cross", cross = cross)

        if (kernel == "linear") {
            fit <- tune(svm, y ~ ., data = x, kernel = "linear", cost = cost, tunecontrol = tune.control)
        } else if (kernel == "radial") {
            if (is.null(gamma)) {
                fit <- tune(svm, y ~ ., data = x, kernel = "radial", cost = cost, tunecontrol = tune.control)
            } else {
                fit <- tune(svm, y ~ ., data = x, kernel = "radial", cost = cost, gamma = gamma, tunecontrol = tune.control)
            }
        } else {
            stop("Unsupported kernel type")
        }
    } else {
        if (kernel == "linear") {
            fit <- svm(y ~ ., data = x, kernel = "linear", cost = cost)
        } else if (kernel == "radial") {
            if (is.null(gamma)) {
                fit <- svm(y ~ ., data = x, kernel = "radial", cost = cost)
            } else {
                fit <- svm(y ~ ., data = x, kernel = "radial", cost = cost, gamma = gamma)
            }
        } else {
            stop("Unsupported kernel type")
        }
    }

    return(fit)
}

