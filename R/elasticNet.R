elasticNet = function(x, y, lambda1, lambda2, max_iter = 1000, tol = 1e-6, intercept = TRUE) {
    x = x[complete.cases(x), ]
    y = y[complete.cases(y), ]
    x = convertCatToNumeric(x, intercept)
    hasCategorical = x$hasCategorical
    x = x$data

    if(!hasCategorical & intercept){
        print("Adding intercept column to matrix.")
        x = cbind(1, x)
    }

    colNames = colnames(x)

    # TO-DO: Enable scaling for better regularization.
    # x = scale(x, center = TRUE, scale = TRUE)
    # x = cbind(1, x)
    # print(head(x))
    # Check if x is a data frame and convert it to a matrix if necessary
    if (is.data.frame(x)) {
        x = as.matrix(x)
    }

    # Check if y is a data frame and convert it to a matrix if necessary
    if (is.data.frame(y)) {
        y = as.matrix(y)
    }
    n = nrow(x)
    p = ncol(x)
    beta = rep(0, p)
    obj_old = Inf

    for (iter in 1:max_iter) {
        beta_old = beta

        # Update beta using Elastic Net penalty
        for (j in 1:p) {
            r = y - x %*% beta + x[, j] * beta[j]
            beta[j] = sign(sum(x[, j] * r)) * max(0, abs(sum(x[, j] * r)) - lambda1) / (lambda2 + sum(x[, j]^2))
        }

        # Calculate the objective function (MSE + regularization)
        obj = sum((y - x %*% beta)^2) + lambda1 * sum(abs(beta)) + lambda2 * sum(beta^2)

        # Check for convergence
        if (abs(obj - obj_old) < tol) {
            break
        }

        obj_old = obj
    }
    result = data.frame(beta)
    row.names(result) = colNames
    return(result)
}
