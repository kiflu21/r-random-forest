library(randomForest)

# Train the randomForest classifier for mtry distinct choices at each node and ntree trees
model = randomForest(V1 ~. , data=lr, mtry=1, ntree=50)

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(model, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(model$err.rate),col=1:4,cex=0.35,fill=1:4)