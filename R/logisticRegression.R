
logisticRegression <- function(x, y, alpha = 0.01, epochs = 1000, threshold = 0.5, intercept = TRUE, epsilon = 1e-8) {
    colNames = colnames(x)
    # print(colNames)
    # Add a column of ones to the predictor matrix for the intercept term

    if(intercept){
        x <- cbind(1, x)
    }

    # Initialize weights with zeros
    weights <- numeric(ncol(x))

    # Gradient descent
    for (epoch in 1:epochs) {
        # Calculate predicted probabilities
        logits <- x %*% weights
        probabilities <- sigmoid(logits)

        # Calculate error
        error <- probabilities - y

        # Update weights
        gradient <- t(x) %*% error
        weights <- weights - alpha * gradient

        # Check convergence
        if (max(abs(gradient)) < epsilon) {
            break
        }
    }

    # Make predictions
    logits <- x %*% weights
    probabilities <- 1 / (1 + exp(-logits))
    predictions <- ifelse(probabilities > threshold, 1, 0)
    result = as.data.frame(weights)
    row.names(result) = c("intercept", colNames)
    return(result)
    # return(list(weights = weights, predictions = predictions, probabilities = probabilities))
}
