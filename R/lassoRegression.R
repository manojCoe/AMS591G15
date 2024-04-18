# Define a function for Lasso regression using coordinate descent
# setting max_iter = 500 for testing, it should default to 1000 in production.
lassoRegression <- function(x, y, lambda, max_iter = 500, tol = 1e-7, intercept = TRUE) {
    x = x[complete.cases(x), ]
    y = y[complete.cases(y), ]
    x = convertCatToNumeric(x, intercept)
    hasCategorical = x$hasCategorical
    x = x$data

    if(!hasCategorical & intercept){
        print("Adding intercept column to matrix.")
        x = cbind(1, x)
    }
    # print(head(x))

    # Check if x is a data frame and convert it to a matrix if necessary
    if (is.data.frame(x)) {
        x <- as.matrix(x)
    }

    # Check if y is a data frame and convert it to a matrix if necessary
    if (is.data.frame(y)) {
        y <- as.matrix(y)
    }
    p <- ncol(x)
    beta <- rep(0, p)
    beta_diff <- Inf
    iter <- 1

    while (iter <= max_iter && beta_diff > tol) {
        beta_old <- beta

        for (j in 1:p) {
            r <- y - x %*% beta + x[, j] * beta[j]
            beta[j] <- sign(sum(x[, j] * r)) * max(0, abs(sum(x[, j] * r)) - lambda) / sum(x[, j]^2)
        }

        beta_diff <- sum((beta - beta_old)^2)
        iter <- iter + 1
    }

    result = data.frame(beta)
    row.names(result) = colnames(x)
    return(result)
}
