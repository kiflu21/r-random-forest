setwd(".")
lr <- read.csv("./lr.data", header=FALSE)

# Import the necessary libraries
library(randomForest)
library(plyr)

# Function to compute 5 fold cross validated randomForest
cvRandomForest = function(trees) {
  k = 5
  n = floor(nrow(lr)/k)
  err.vec = vector()

  for (i in 1:k) {
    s1 = ((i - 1) * n)
    s2 = (i * n)
    subset = s1:s2

    # Create training and test sets
    data.test = lr[subset, ]
    data.train = lr[-subset, ]

    # Create a random forest model
    set.seed(i * 143)
    model = randomForest(x = data.train[, -1], y = as.factor(data.train[, 1]), ntree=trees, mtry=1)

    # Predict for the test set using the generated model
    prediction = predict(model, newdata = data.test[, -1])

    # Compute the error
    err.vec[i] = count(data.test$V1 != prediction)[2,2]
  }

  # Return the error
  mean(err.vec)
}

errors = vector()
upperlimit = 500
indexes = seq(2, upperlimit, 10)

# Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(length(indexes))

# Loop through
for (i in indexes) {
  errors = c(errors, cvRandomForest(i))
  progress.bar$step()
}
