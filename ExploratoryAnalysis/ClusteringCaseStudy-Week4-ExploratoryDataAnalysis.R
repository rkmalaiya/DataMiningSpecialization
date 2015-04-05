setwd("C:\\Users\\rkmalaiya\\Documents\\GitHub\\DataMiningSpecialization\\ExploratoryAnalysis")
load("samsungData.rda")
str(samsungData)
table(samsungData$activity)

## Lets consider first subject
par(mfrow= c(1,2), mar = c(5,4,1,1))
require(plyr)
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
## plotting average accelaration 
plot(sub1[,1], col= sub1$activity, ylab = names(sub1)[1])
plot(sub1[,2], col= sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

## plotting maximum accelaration
par(mfrow= c(1,2))
plot(sub1[,10], col= sub1$activity, ylab = names(sub1)[10])
plot(sub1[,11], col= sub1$activity, ylab = names(sub1)[11])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

## Lets try hcluster
dm <- dist(sub1[,10:12])
hc <- hclust(dm)
plot(hc, col = unclass(sub1$activity))

install.packages("sparcl")
library(sparcl)
y = cutree(hc,h = 1)
ColorDendrogram(hc, y=y, labels = names(y))


svd1 <- svd(scale(sub1[,-c(562,563)]))
par(mfrow = c(1,2))
plot(svd1$u[,1], col = sub1$activity, pch = 19)
plot(svd1$u[,2], col = sub1$activity, pch = 19)
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

## since none above gives a clear indication of the activities, lets find the maximum contributer parameters.
plot(svd1$v[,2], pch = 19)

maxContrib <- which.max(svd1$v[,2])
dm <- dist(sub1[,c(10:12, maxContrib)])
hc <- hclust(dm)
ColorDendrogram(hc, y=y, labels = names(y))
names(samsungData[maxContrib])

## Let's now try with Kmeans Clustering
kclust <- kmeans(sub1[, -c(562,563)], centers = 6) ## -c(562,563) because we want to avoid last 2 columns
str(kclust$cluster)
class(kclust$cluster)
str(sub1$activity)
table(kclust$cluster, sub1$activity)

# trying again
kclust <- kmeans(sub1[, -c(562,563)], centers = 6, nstart = 1)
table(kclust$cluster, sub1$activity)

# trying again
kclust <- kmeans(sub1[, -c(562,563)], centers = 6, nstart = 100)
table(kclust$cluster, sub1$activity)

# trying again and again and keep trying
kclust <- kmeans(sub1[, -c(562,563)], centers = 6, nstart = 100)
table(kclust$cluster, sub1$activity)

# Trying to understand which featues are most contributing
# Centers for Standing
plot(kclust$centers[1,1:10], pch = 19, ylab = "Cluster Center", xlab = "")

# Centers for walking
plot(kclust$centers[4,1:10], pch = 19, ylab = "Cluster Center", xlab = "")



