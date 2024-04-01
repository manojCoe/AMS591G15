ridgeRegression <- function(x, y, lambda) {

    colNames = colnames(x)
    x = convertCatToNumeric(x)

    # Check if x is a data frame and convert it to a matrix if necessary
    if (is.data.frame(x)) {
        x <- as.matrix(x)
    }

    # Check if y is a data frame and convert it to a matrix if necessary
    if (is.data.frame(y)) {
        y <- as.matrix(y)
    }

    # Compute the dimensions of x
    n <- dim(x)[1]
    p <- dim(x)[2]

    # # Compute coefficients using the normal equation
    beta <- solve(t(x) %*% x + lambda * diag(p), t(x) %*% y)

    # Convert beta to a data frame with the specified column names
    result <- data.frame(beta)

    return(result)
}
