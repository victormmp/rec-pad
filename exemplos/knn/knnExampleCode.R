rm(list = ls())
library('plot3D')

# Funtion responsible to calculate the N neighbours
myknn <- function(X, Y, k, xt) {
    N <- nrow(X)
    dxt <- matrix(nrow = N, ncol = 1)
    
    for (i in 1:N) {
        dxt[i] <- sum((xt - X[i, ]) ^ 2)
    }
    
    NN <- order(dxt)
    cxt <- sign(sum(Y[NN[1:k]]))
    
    return(cxt)
}

k <- 3
N12 <- 50 # sempre par
xc1 <- matrix(rnorm(N12 * 2, mean = 2, sd = 1), ncol = 2)
xc2 <- matrix(rnorm(N12 * 2, mean = 4, sd = 1), ncol = 2)

X <- rbind(xc1, xc2)
Y <- rbind(-1 * matrix(1, nrow = N12, ncol = 1), matrix(1, nrow = N12, ncol = 1))

plot(xc1[, 1], xc1[, 2], col = 'red', xlim = c(0, 6), ylim = c(0, 6))

par(new = T)

plot(xc2[, 1], xc2[, 2], col = 'blue', xlim = c(0, 6), ylim = c(0, 6))

xyseq <- seq(0, 6, 0.1)
M <- matrix(nrow = length(xyseq), ncol = length(xyseq))

ci <- 0

for (i in xyseq) {
    ci <- ci + 1
    cj <- 0
    
    for (j in xyseq) {
        cj <- cj + 1
        xt <- as.matrix(c(i, j))
        M[ci, cj] <- myknn(X, Y, k, xt)
    }
}

persp3d(xyseq, xyseq, M,col='red')
persp3D(xyseq, xyseq, M)
scatter3D(xc1[, 1], xc1[, 2], matrix(0, nrow = N12, ncol = 1), add = T)
scatter3D(xc2[, 1],  xc2[, 2],  matrix(0, nrow = N12, ncol = 1), add = T, col = 'red')

