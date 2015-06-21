
library(caret)
set.seed(1175)
pml.training <- read.csv("../project-writeup/data/pml-training.csv")  
#View(pml.training)
pml.testing <- read.csv("../project-writeup/data/pml-testing.csv")
#View(pml.testing)

#summary(pml.training)

# drop all variables where there are too many NAs
training <- pml.training[, colSums(is.na(pml.training)) < 19000]
# drop the observation number, user_name and timestamps from the data
training <- training[, -c(1:2,3:7)]
# drop all variables where there are too many blanks
training <- training[, colSums(training != "") > 1000]

# split training into train and test set
inTrain <- createDataPartition(y=training$classe, p=0.65, list=FALSE)
train <- training[inTrain,]
vald <- training[-inTrain,]

M <- abs(cor(train[,!colnames(train) %in% c("classe")]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

preProc <- preProcess(train[,!colnames(train) %in% c("classe")],method="pca")
trainPC <- predict(preProc,train[,!colnames(train) %in% c("classe")])

modelFitRf <- train(as.factor(train2$classe) ~ .,method="rf",data=trainPC, ntrees=100)
valdPC <- predict(preProc, vald[,!colnames(vald) %in% c("classe")])
confusionMatrix(vald$classe,predict(modelFitRf,valdPC))


missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}
errRate = missClass(vald$classe, predict(modelFitRf,valdPC))
modelFitRf$finalModel

# prepare the test set
testing <- pml.testing
testing$classe <- 0
testing <- testing[,colnames(test)]
testing$classe <- NULL
testing$user_name <- NULL
# apply PCA
testingPC <- predict(preProc, testing)

my_pred <- predict(modelFitRf,testingPC)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
# write out all predictions
pml_write_files(my_pred)
