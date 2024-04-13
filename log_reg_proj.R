# Generate some sample data
set.seed(123)
n <- 100
x <- rnorm(n)
y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 * x + 0.1)) + 0.1 * rnorm(n)))

# Initialize coefficients
beta <- c(0, 0)

# Sigmoid function
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Cost function
cost <- function(beta, x, y) {
  N <- length(y)
  y_hat <- sigmoid(beta[1] + beta[2] * x)
  -sum(y * log(y_hat) + (1 - y) * log(1 - y_hat)) / N
}

# Gradient descent
gradient_descent <- function(beta, x, y, alpha, iterations) {
  N <- length(y)
  cost_history <- numeric(iterations)
  
  for (i in 1:iterations) {
    y_hat <- sigmoid(beta[1] + beta[2] * x)
    
    gradient <- c(sum(y_hat - y) / N, sum((y_hat - y) * x) / N)
    
    beta <- beta - alpha * gradient
    cost_history[i] <- cost(beta, x, y)
  }
  
  list(beta = beta, cost_history = cost_history)
}

# Run gradient descent
result <- gradient_descent(beta, x, y, alpha = 0.01, iterations = 1000)

# Print coefficients and cost history
print(result$beta)
plot(result$cost_history, type = 'l', xlab = 'Iterations', ylab = 'Cost')

#########################################################################


