# Custom VIF function
custom_vif <- function(x, y, threshold = 4) {
    # Convert x to a data frame
    x_df <- as.data.frame(x)

    # Calculate the VIF for each explanatory variable
    vif_values <- sapply(colnames(x_df), function(var) {
        x_subset <- x_df[, c(var, colnames(x_df)[colnames(x_df) != var])]
        rsq <- summary(lm(as.formula(paste(var, "~ .")), data = x_subset))$r.squared
        vif <- ifelse(rsq == 1, Inf, 1 / (1 - rsq))
        return(vif)
    })

    print("VIF values:")
    print(vif_values)

    max_vif <- max(vif_values)
    if (max_vif > threshold) {
        max_vif_var <- names(vif_values)[which.max(vif_values)]
        cat("Removing variable:", max_vif_var, "with VIF =", max_vif, "\n")

        x <- x[, colnames(x) != max_vif_var]
        return(custom_vif(x, y, threshold))
    } else {
        cat("Collinearity reduced below threshold.\n")
        return(list(x = x, y = y))
    }
}

# Example usage with x and y
set.seed(123)
n <- 50
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- x1 + x2 + rnorm(n, sd = 0.2)
x4 <- rnorm(n)
y <- x1 - x2 - x3 + x4 + rnorm(n)

x_matrix <- cbind(x1, x2, x3, x4)
result <- custom_vif(x_matrix, y)
final_x <- result$x
final_y <- result$y

# Now you can use final_x and final_y for further analysis

