library(caret, quietly = T, warn.conflicts = F)
library(e1071, quietly = T, warn.conflicts = F)
if(!require(RANN)) {
  install.packages("RANN")
  require(RANN)
}
if(!require(doParallel)) {
  install.packages("doParallel")
  require(doParallel)
}
if(!require(klaR)) {
  install.packages("klaR")
  require(klaR)
}
if(!require(MASS)) {
  install.packages("MASS")
  require(MASS)
}

library(dplyr)

pmlTrain <- read.csv("pml-training.csv")
pmlTest <- read.csv("pml-testing.csv")


pmlTrain <- pmlTrain[, -(1:7)]

pmlTrain_temp <- data.frame(sapply(select(pmlTrain, -classe), as.numeric)) 
pmlTrain_temp$classe <- pmlTrain$classe
pmlTrain <- pmlTrain_temp


sum(is.na(pmlTrain))

na_cols <- apply(X = pmlTrain, MARGIN = 2, function(x) {ifelse(sum(is.na(x)) > 0,TRUE,FALSE)})
#class(na_cols)
#na_cols

pmlTrain <- pmlTrain[,!na_cols]

unique(pmlTrain$classe)

library(plyr)

ddply(pmlTrain, .(classe), function(x) {sum(is.na(x))})

apply(X = pmlTrain, MARGIN = 2, function(x){sum(is.na(x))})

highlyCor <- findCorrelation(select(pmlTrain, -classe), exact = F)
pmlTrain <- pmlTrain[ , - highlyCor[complete.cases(highlyCor)]]

#preObj <- preProcess.default(select(pmlTrain, -classe), na.remove=T)

#pmlTrain_pp <- predict(preObj, select(pmlTrain, -classe))
pmlTrain_pp <- pmlTrain


registerDoParallel(cores=4)

library(mlbench)

control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(select(pmlTrain, -classe), pmlTrain$classe, sizes=c(1:8), rfeControl=control)



results
predictors(results)[1:5]
# plot the results
plot(results, type=c("g", "o"))

pmlTrain_pp <- pmlTrain[, predictors(results)[1:2]]
pmlTrain_pp$classe <- pmlTrain$classe


trainObj <- train(classe~., data = pmlTrain_pp, method = "rf")

pmlTrain_predict <- predict(trainObj)
cnfM <- confusionMatrix(pmlTrain$classe, pmlTrain_predict)
cnfM

install.packages("mlbench")



data(PimaIndiansDiabetes)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

featurePlot(select(pmlTrain, -classe)[, 1:5], pmlTrain$classe)

inTrain <- createDataPartition(y = pmlTrain$classe, p = .7, list = F)
pmlTrain_rel<- pmlTrain[inTrain,]
pmlTest<- pmlTrain[-inTrain,]

