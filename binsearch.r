setwd("~/Documents/Academic/ML/RF")
lr <- read.csv("~/Documents/Academic/ML/RF/lr.data", header=FALSE)

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
    set.seed(i * 69)
    model = randomForest(x = data.train[, -1], y = as.factor(data.train[, 1]), ntree=trees, mtry=1)
    
    # Predict for the test set using the generated model
    prediction = predict(model, newdata = data.test[, -1])
    
    # Compute the error
    err.vec[i] = count(data.test$V1 != prediction)[2,2]
  }
  
  # Return the error
  print(paste("Trees", trees, "Error", mean(err.vec)))
  mean(err.vec)
}

# Run binary search
threshold = 1.05
low = 2
high = 500
minError = cvRandomForest(high)
while(low <= high)
{
  mid = floor((low + high) / 2);
  rfMid = cvRandomForest(mid)
  rfNext = cvRandomForest(mid - 1)
  if(rfMid < threshold * minError && rfNext > threshold * minError) {
    print(paste("Mid", mid, "Error", rfMid))
    break
  } else if (rfMid < threshold * minError && rfNext < threshold * minError) {
    high = mid - 1
  } else {
    low = mid + 1
  }
}