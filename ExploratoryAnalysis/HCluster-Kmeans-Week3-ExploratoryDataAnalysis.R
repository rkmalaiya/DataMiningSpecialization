set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
mydf <- data.frame(x = x, y = y)
distxy <- dist(mydf)
## Hierarchical Cluster
hcluster <- hclust(distxy)
plot(hcluster)
## drawing heat map
set.seed(143)
mydm <- as.matrix(mydf)[sample(1:12),]
heatmap(mydm)

#KMeans cluster
kmeansobj <- kmeans(mydf,centers = 3)
names(kmeansobj)
str(kmeansobj)
par(mar = repo(0.2,4))
plot(x,y,col = kmeansobj$cluster, pch = 19, cex = 2 )
points(kmeansobj$centers, col =1:3, pch = 3, cex = 3, lwd = 3)

## heatmap for kmeans
par(mfrow = c(1,2), mar = c(2,4,0.1,0.1))
image(t(mydf)[, nrow(mydf):1], yaxt = "n")
image(t(mydf)[, order(kmeansobj$cluster):1], yaxt = "n")
