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
#pmlTrain_temp$classe <- pmlTrain$classe
pmlTrain_pp <- pmlTrain_temp

#featurePlot(pmlTrain_pp[, 1:5], pmlTrain$classe,  plot = "pairs")
#featurePlot(pmlTrain_pp[, 1:5], pmlTrain$classe,  plot = "box")

inTrain <- createDataPartition(y=pmlTrain$classe, list = FALSE, p = .7)

pmlTrain_pp_train <- pmlTrain_pp[inTrain,]
pmlTrain_pp_test <- pmlTrain_pp[-inTrain,]
classe_train <- pmlTrain[inTrain, "classe"]
classe_test <- pmlTrain[-inTrain, "classe"]

preObj <- preProcess(pmlTrain_pp_train, method = c("center", "scale"))

pmlTrain_pp_train <- predict(preObj, pmlTrain_pp_train)
pmlTrain_pp_test <- predict(preObj, pmlTrain_pp_test)


#featurePlot(pmlTrain_pp_train[, 1:20], classe_train)
#featurePlot(pmlTrain_pp_train[, 20:40], classe_train)
#featurePlot(pmlTrain_pp_train[, 40:60], classe_train)
#featurePlot(pmlTrain_pp_train[, 60:80], classe_train)
#featurePlot(pmlTrain_pp_train[, 80:100], classe_train)
#featurePlot(pmlTrain_pp_train[, 100:120], classe_train)
#featurePlot(pmlTrain_pp_train[, 120:140], classe_train)
#featurePlot(pmlTrain_pp_train[, 140:152], classe_train)

require(doParallel)
library(ggplot2)

registerDoParallel(cores=4)
control <- trainControl(method="repeatedcv", number=2, repeats=2)

#folds <- createMultiFolds(classe[inTrain])
folds <- createFolds(classe_train)

#model <- train(classe~., data = pmlTrain_pp_train[folds$Fold02,], method="vbmpRadial", trControl=control)
library(dplyr)
library(plyr)
ddply(cbind(pmlTrain_pp_train[folds$Fold02,], classe = classe_train[folds$Fold02]), .(classe), function(x) {sum(is.na(x))})

ddply(cbind(pmlTrain_pp_train[folds$Fold02,], classe = classe_train[folds$Fold02]), .(classe), function(x) {dim(x)[1] * dim(x)[2]})

model <- train.default(x=pmlTrain_pp_train[folds$Fold02,], y = classe_train[folds$Fold02], method = "glm")
model <- train.default(x=pmlTrain_pp_train, y = classe_train, method = "nb")

train_data <- cbind(pmlTrain_pp_train, classe = classe_train)


model <- train(classe ~ . ,data = train_data, method = "bayesglm")

tt2 <- predict(model, pmlTrain_pp_test[, !names(pmlTrain_pp_test) %in% c("classe")])
plot(tt2)
tt2

lm <- naiveBayes(classe ~ ., data = train_data)
lm

tt1 <- predict(lm, pmlTrain_pp_test)

table(tt1, classe_test)


str(train_data$classe)

plot(model)

install.packages("randomForest")
library(randomForest)
lm <- randomForest(classe ~ ., data = train_data)

install.packages("RWeka")


library(kernlab)
lm <- svm(classe ~ ., data = train_data, scale = F)
lm






