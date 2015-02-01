# Read data
setwd(".")
lr <- read.csv("./lr.data", header=FALSE)

# Import libraries
library(randomForest)
library(plyr)

# Function to compute the elbow of a x% RF
pRandomForest = function(x) {
  eRF = function(x, trees) {
    # Stratified Sampling
    # We define a population which has all elements of type '#'
    # we then make sure p% percentage of population is used for sampling

    # Get x% of the population
    p = (x * nrow(lr))/100
    population = as.factor(rep('#', nrow(lr)))

    # k-fold validation
    k = 5
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
      model = randomForest(x = data.train[, -1], y = as.factor(data.train[, 1]), strata=population, sampsize=c(p), replace=FALSE, importance=TRUE, ntree=trees, mtry=1)

      # Predict for the test set using the generated model
      prediction = predict(model, newdata = data.test[, -1])

      # Compute the error
      err.vec[i] = count(data.test$V1 != prediction)[2,2]
    }

    # Return the error
    mean(err.vec)
  }

  # Run binary search
  threshold = 1.05
  low = 2
  high = 500
  minError = eRF(x, high)
  elbow = 0
  while(low <= high)
  {
    mid = floor((low + high) / 2);
    rfMid = eRF(x, mid)
    rfNext = eRF(x, mid - 1)
    if(rfMid < threshold * minError && rfNext > threshold * minError) {
      elbow = mid
      break
    } else if (rfMid < threshold * minError && rfNext < threshold * minError) {
      high = mid - 1
    } else {
      low = mid + 1
    }
  }

  # Return the elbow
  elbow
}

# The sequences through which we need to iterate
sequence = seq(10, 80, 10)

# Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(length(sequence))

elbows = vector()
for (i in sequence) {
  elbows = c(elbows, pRandomForest(i))
  progress.bar$step()
}
