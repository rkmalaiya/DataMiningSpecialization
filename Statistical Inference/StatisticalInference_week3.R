data(sleep)
head(sleep)
## with(sleep, plot(x=group, y = extra))

g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
diff <- g2-g1

mean <- mean(diff)
sd <- sd(diff)
n <- 10

mean + c(-1,1) * qt(.975, n-1) * sd/sqrt(n)

t.test(diff)
t.test(g2,g1,paired = TRUE)
t.test(extra ~ I(relevel(group,2)), paired = TRUE, data = sleep)
