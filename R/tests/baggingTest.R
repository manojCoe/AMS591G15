
data("iris")
x = model.matrix(Species ~ ., data = iris)[, -1]
y = iris$Species
data = data.frame(x, y)
y = as.factor(y)
res = bagging(x, y, y~x, model_type = "logistic", R = 10, type = "class", lambda = 0.01, bagging_type = "majority_vote")
