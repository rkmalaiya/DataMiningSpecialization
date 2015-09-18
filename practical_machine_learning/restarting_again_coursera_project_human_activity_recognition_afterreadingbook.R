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
library(data.table)

pmlTrain <- fread("pml-training.csv")
pmlTest <- fread("pml-testing.csv")

## Let's look at data
pmlTrain <- tbl_dt(pmlTrain)

dim(pmlTrain)

names(pmlTrain)

summary(pmlTest)

## Through above quick glance, we can see that there are many variables which are not having values. 
## Further observation shows that many variables have values only when new_window variable is Yes. Let's observe data for new_window = Yes and No.

pmlTrain_yes <- filter(pmlTrain, new_window == 'yes', num_window == 2)
pmlTrain_no <- filter(pmlTrain, new_window == 'no', num_window == 2)

head(pmlTrain_yes, n= 1)
head(pmlTrain_no, n = 2)

## We can clearly 

## Let's remove the columns which may noy be necessary for Model Learning purposes.

## 1) First 7 columns are housekeeping data and can be ignored
## 2) As we observed above, there are few columns which are summary columns and may not be fit for Model Learning purposes.
## Summary columns are kurtosis_, skewness_, max_, min_, amplitude_, var_total_,  avg_, stddev_, var_, 


pmlTrain_sel <- pmlTrain[ ,-(1:7), with = F]

pmlTrain_sel <- dplyr::select(pmlTrain_sel, -contains("kurtosis_"), -contains("skewness_"), -contains("max_"), -contains("min_"),
                              -contains("amplitude_"), -contains("var_total_"), -contains("avg_"), -contains("stddev_"), -contains("var_") )

pmlTest_sel <- pmlTest[, names(pmlTrain_sel), with=F]

classe <- as.factor(pmlTrain_sel$classe)
pmlTrain_sel <- data.frame(sapply(select(pmlTrain_sel, -classe), as.numeric)) 


#classe <- as.factor(pmlTest_sel$classe)
pmlTest_sel <- data.frame(sapply(select(pmlTest_sel, -classe), as.numeric)) 
#pmlTest_sel$classe <- classe

#Lets check for outliers
boxplot(pmlTrain_sel)

cor <- cor(pmlTrain_sel, use = "p")
hist(cor)

## Since we dont see much outliers n data is normalized hence only scaling

preObj <- preProcess(pmlTrain_sel, method = c("center","scale"))

pmlTrain_prep <- predict(preObj, pmlTrain_sel)
pmlTest_prep <- predict(preObj, pmlTest_sel)

registerDoParallel(cores=2)
control <- trainControl(method="repeatedcv", number=2, repeats=2)


## Train and Plot a model
#pmlTrain_prep$classe <- classe
modelNB <- train(classe~., data=pmlTrain_prep, method="nb", trControl=control)
save(modelNB, file = "modelNB.rda")
plot(modelNB)
pmlTrain_pred <- predict(modelNB)
confusionMatrix(classe, pmlTrain_pred)


modelavNNET <- train(classe~., data=pmlTrain_prep, method="avNNet", trControl=control)
save(modelavNNET, file = "modelavNNET.rda")
plot(modelavNNET)
pmlTrain_pred <- predict(modelavNNET)
confusionMatrix(classe, pmlTrain_pred)

modelSVM <- train(classe~., data=pmlTrain_prep, method="svmRadial", trControl=control)
save(modelSVM, file = "modelSVM.rda")
plot(modelSVM)
pmlTrain_pred <- predict(modelSVM)
confusionMatrix(classe, pmlTrain_pred)

modelrf <- train(classe~., data=pmlTrain_prep, method="rf", trControl=control)
save(modelrf, file = "modelRF.rda")
plot(modelrf)
pmlTrain_pred <- predict(modelrf)
confusionMatrix(classe, pmlTrain_pred)



results <- resamples(list(nb=modelNB, avNNet=modelavNNET, SVM=modelSVM, rf=modelrf))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

pmlTest_pred_nb <- predict(modelNB, newdata = pmlTest_sel)
pmlTest_pred_nnet <- predict(modelavNNET, newdata = pmlTest_sel)
pmlTest_pred_svm <- predict(modelSVM, newdata = pmlTest_sel)
pmlTest_pred_rf <- predict(modelrf, newdata = pmlTest_sel)

pmlTest_pred_nb
pmlTest_pred_nnet
pmlTest_pred_svm
pmlTest_pred_rf

