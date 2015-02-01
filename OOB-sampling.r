# Read data
setwd("~/Documents/Academic/ML/RF")
lr <- read.csv("~/Documents/Academic/ML/RF/lr.data", header=FALSE)

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
    set.seed(111 * trees)
    p = (x * nrow(lr))/100
    population = as.factor(rep('#', nrow(lr)))
    model = randomForest(V1~., data=lr, strata=population, sampsize=c(p), replace=FALSE, ntree=trees, mtry=1, importance=TRUE)
    error = model$err.rate[trees, "OOB"]
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
