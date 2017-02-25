#Short file to test memory size of objects for memory optimization
library(Matrix)


x <- rnorm(1e4, mean=10, sd=2)
x1 <- as.integer(rnorm(1e4, mean=10, sd=2))
p <- matrix(nrow = 1e2, ncol = 1e2, 0)
p1 <- matrix(0L, 100, 100)
p2 <- data.frame(p1)


for (t in 1:(length(x1) - 1)) p[x1[t], x1[t + 1]] <- p[x1[t], x1[t + 1]] + 1
for (t in 1:(length(x1) - 1)) p1[x1[t], x1[t + 1]] <- p1[x1[t], x1[t + 1]] + 1L
for (t in 1:(length(x1) - 1)) p2[x1[t], x1[t + 1]] <- p2[x1[t], x1[t + 1]] + 1L
for (i in 1:4) p[i, ] <- p[i, ] / sum(p[i, ])
p3 <- as.matrix(p2)


print(paste("x :", object.size(x)))
print(paste("x1:", object.size(x1)))

print(paste("p :", object.size(p)))
print(paste("p1:", object.size(p1)))
print(paste("p2:", object.size(p2)))
print(paste("p3:", object.size(p3), ", typeof:", typeof(p3)))