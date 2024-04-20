# Custom VIF function
custom_vif <- function(model, threshold = 6, intercept = FALSE) {
    scaled_data <- scale(model$model[, -1])
    formula_prefix <- if (intercept) "" else "-"
    # scaled_data = apply(model$model[, -1], 2, function(x) (x - min(x)) / (max(x) - min(x)))
    colnames(scaled_data) <- colnames(model$model[, -1])
    vif_values <- sapply(colnames(scaled_data), function(var) {
        rsq <- summary(lm(as.formula(paste(var, "~ . ", formula_prefix, var)), data = model$model))$r.squared
        vif <- ifelse(rsq == 1, Inf, 1 / (1 - rsq))
        return(vif)
    })
    print("VIF values:")
    print(vif_values)
    max_vif <- max(vif_values)
    if (max_vif > threshold) {
        max_vif_var <- names(vif_values)[which.max(vif_values)]
        cat("Removing variable:", max_vif_var, "with VIF =", max_vif, "\n")
        model <- update(model, as.formula(paste(". ~ . ", formula_prefix, max_vif_var)))
        print(model$call)
        return(custom_vif(model, threshold))
    } else {
        cat("Collinearity reduced below threshold.\n")
        return(model)
    }
}

set.seed(123)
n <- 50
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- x1 + x2 + rnorm(n, sd = 0.2)
x4 <- rnorm(n)
y <- x1 - x2 - x3 + x4 + rnorm(n)


x = cbind(x1,x2,x3,x4)
# x = scale(x)
# y = data.frame(y)

model = lm(y ~ x1 + x2 + x3 + x4)
custom_vif(model)

data = data.frame(y, x)
model2 = lm(y ~ ., data)
res = custom_vif(model2)

