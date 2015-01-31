# Stratified Sampling

# We define a population which has all elements of type '#'
# we then make sure p% percentage of population is used for sampling
library(randomForest)
p = (30 * nrow(lr))/100
population = as.factor(rep('#', nrow(lr)))
model = randomForest(V1~., data=lr, keep.inbag=TRUE, strata=population, sampsize=c(p), replace=FALSE, ntree=50, mtry=1, importance=TRUE)