setwd(".")
lr <- read.csv("./lr.data", header=FALSE)

# Import libraries
library(randomForest)

# Parameters
trees = 105

# Train the randomForest classifier for mtry distinct choices at each node and ntree trees
model = randomForest(V1 ~. , data=lr, mtry=1, ntree=trees, importance=TRUE)

# Get the OOB error
print(model$err.rate[trees, "OOB"])
