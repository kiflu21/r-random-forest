# Read data
setwd(".")
lr <- read.csv("./lr.data", header=FALSE)

# Import libraries
library(randomForest)
library(plyr)

# Parameters
trees = floor(105 * 1.25)
k = 5

# Compute the 5-fold cross validation error
errors = vector()
indexes = seq(1, 10, 2)
for (m in indexes) {
    n = floor(nrow(lr)/k)
    err.vec = vector()

    for (fold in 1:k) {
        s1 = ((fold - 1) * n)
        s2 = (fold * n)
        subset = s1:s2

        # Create training and test sets
        data.test = lr[subset, ]
        data.train = lr[-subset, ]

        # Create a random forest model
        model = randomForest(x = data.train[, -1], y = as.factor(data.train[, 1]), importance=TRUE, ntree=trees, mtry=m)

        # Predict for the test set using the generated model
        prediction = predict(model, newdata = data.test[, -1])

        # Compute the error
        err.vec[fold] = count(data.test$V1 != prediction)[2,2]
    }

    # Return the error
    errors = c(errors, mean(err.vec))
}
