rmse = function(observed, predicted) {
    sqrt(mean((observed - predicted)^2))
}
