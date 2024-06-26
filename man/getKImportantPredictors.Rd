% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{getKImportantPredictors}
\alias{getKImportantPredictors}
\title{Get K Important Predictors}
\usage{
getKImportantPredictors(x, y, type = "default", k = 6)
}
\arguments{
\item{x}{The predictor variables.}

\item{y}{The response variable.}

\item{type}{The type of analysis ("default" or "class").}

\item{k}{The number of important predictors to select.}
}
\value{
A character vector of the top K important predictors.
}
\description{
Retrieves the top K important predictors using Random Forest feature importance.
}
\examples{
# Load required library
library(randomForest)
# Generate sample data
set.seed(123)
x <- matrix(rnorm(100), ncol = 5)
y <- sample(0:1, 20, replace = TRUE)
# Get top 3 important predictors
important_predictors <- getKImportantPredictors(x, y, type = "default", k = 3)
# Display the top important predictors
print(important_predictors)
}
