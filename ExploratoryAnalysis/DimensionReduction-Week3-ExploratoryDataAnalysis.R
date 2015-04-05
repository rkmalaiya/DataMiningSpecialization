## no interesting pattern found
set.seed(12345)
par(mar=rep(0.2,4))
dm <- matrix(rnorm(400), nrow = 40)
head(dm)
str(dm)
head(t(dm))
tdm <- t(dm)
t(dm)[,nrow(dm):1] # this command reverses the rows
image(1:10, 1:40, t(dm)[,nrow(dm):1])
heatmap(dm)

## lets have an interesting pattern
set.seed(678910)
for(i in 1:40) {
  #flip a coin
  coinflip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  
  if(coinflip) {
    dm[i,] <- dm[i,] + rep(c(0,3), each = 5)
  }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dm)[, nrow(dm):1])
image(1:10, 1:40, t(dm))
heatmap(dm)

hh <- hclust(dist(dm))
str(hh)
dmo <- dm[hh$order, ]
par(mfrow = c(1,4))
image(t(dmo)[,nrow(dmo):1])

plot(rowMeans(dmo), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dmo), xlab = "Column", ylab = "Column Mean", pch  = 19)


## Use of SVD to reduce dimensions
head(dmo)
scale(dmo)
svd1 <- svd(scale(dmo))
par(mfrow = c(1,4))
image(dmo)
image(t(dmo)[, nrow(dmo):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First Left Sigular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "first right singular vector", pch = 19)


##SVD doesn't work on missing values
dm2 <- dmo
dm2[sample(1:100, size = 40, replace=FALSE)] <- NA
svd2 <- svd(scale(dm2))
install.packages("impute")
library(impute)
load("face.rda")
image(t(faceData)[,nrow(faceData):1])

# lets check variation
svd1 <- svd(scale(faceData))
plot(svd1$d^2 / sum(svd1$d^2), pch = 19, xlab = "Singular Vector", ylab = "Variance Explained")

approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]


par(mfrow = c(1,4))



image(ta)