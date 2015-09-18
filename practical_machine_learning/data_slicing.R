install.packages("caret")
library(caret)
library(kernlab)

data(spam)

inTrain <- createDataPartition(y = spam$type, p=0.75, list=F)

training <- spam[inTrain,]
testing <- spam[-inTrain, ]

dim(training)
str(training)
summary((training))


"This is like running createDataPartition 10 different times"
folds <- createFolds(y=spam$type, k = 10, returnTrain = T)


folds <- createResample(y = spam)

'Plotting Predictors'
install.packages('ISLR')
library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)



install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
boxplot(training$Superplasticizer)
head(training)
(training$Superplasticizer)
