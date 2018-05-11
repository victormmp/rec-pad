rm(list=ls())

cat("\014")
# library("tictoc")
library("rgl")
library("plot3D")

cat("===== Starting Routine=====\n\n")

cat(">> Creating functions\n")

kMeans <- function (X, k, maxit) {
  N <- dim(X)[1]
  n <- dim(X)[2]
  
  seqN <- seq(1,N,1)
  seqk <- seq(1,k,1)
  
  Mc <- matrix(nrow=k, ncol=n)
  Clustx <-matrix(nrow=N, ncol=1)
  
  seqx <- sample(N,k)
  
  Mc <- X[seqx,]
  
  it <- 1
  
  while(it <= maxit){
    for (i in seqN) {
      xrep <- matrix(X[i,], nrow=k, ncol=n, byrow=T)
      vecdist <- rowSums((Mc-xrep)^2)
      Clustx[i] <- which.min(vecdist)
    }
    
    for (j in seqk) {
      xj <- which(Clustx==j)
      Mc[j,] <- colMeans(X[xj,])
    }
    it <- it+1
  }
  
  nam <- c("Mc","Clustx")
  retlist <- list(Mc,Clustx)
  names(retlist) <- nam
  
  return(retlist)
}


pdfnvar <- function(x,m,K,n) {

  y <- ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
  return(y)
  
}


cat(">> Creating samples\n")

nSamples <- 1000
# Number of clusters
k <- 3
# Max Iteration
maxit <- 100

xg1 <- matrix(rnorm(nSamples,mean=2,sd=1),ncol=2)
xg2 <- matrix(rnorm(nSamples,mean=4,sd=0.5),ncol=2)
xg3 <- matrix(rnorm(nSamples,mean=0,sd=0.6),ncol=2)

X <- rbind(xg1,xg2,xg3)

plotLim <- c(-10,10)

# plot(xg1[,1],xg1[,2],xlim=plotLim,ylim=plotLim)
# par(new=T)
# plot(xg2[,1],xg2[,2],xlim=plotLim,ylim=plotLim)
# par(new=T)
# plot(xg3[,1],xg3[,2],xlim=plotLim,ylim=plotLim)

cat(">> Calculatint centers\n")
McList <- kMeans(X,k,maxit)
Mc <- McList$Mc
Clustx <- McList$Clustx

ixg1 <- which(Clustx == 1)
ixg2 <- which(Clustx == 2)
ixg3 <- which(Clustx == 3)

print(Mc)

plot(X[ixg1,1],X[ixg1,2],xlim=plotLim,ylim=plotLim,col=2)
par(new=T)
plot(X[ixg2,1],X[ixg2,2],xlim=plotLim,ylim=plotLim,col=3)
par(new=T)
plot(X[ixg3,1],X[ixg3,2],xlim=plotLim,ylim=plotLim,col=4)
par(new=T)
plot(Mc[1,1],Mc[1,2],xlim=plotLim,ylim=plotLim,col=1,pch=15)
par(new=T)
plot(Mc[2,1],Mc[2,2],xlim=plotLim,ylim=plotLim,col=4, pch=15)

# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r



###########

cat(">> Generating 3D plot\n")

seqx1x2 <- seq(-10,10,0.1)
lseq <- length(seqx1x2)
M1 <- matrix(nrow=lseq,ncol=lseq)
m1 <- colMeans(X[ixg1,])
K1 <- cov(X[ixg1,])
  
  
M2 <- matrix(nrow=lseq,ncol=lseq)
m2 <- colMeans(X[ixg2,])
K2 <- cov(X[ixg2,])

M3 <- matrix(nrow=lseq,ncol=lseq)
m3 <- colMeans(X[ixg3,])
K3 <- cov(X[ixg3,])


M12 <- matrix(nrow=lseq,ncol=lseq)


cr <- 0

for (i in 1:lseq) {
  for(j in 1:lseq) {
    cr <- cr +1
    x1 <- seqx1x2[i]
    x2 <- seqx1x2[j]
    x1x2 <- matrix(cbind(x1,x2),ncol=1)
    M1[i,j] <- pdfnvar(x1x2,m1,K1,2)
    M2[i,j] <- pdfnvar(x1x2,m2,K2,2)
    M3[i,j] <- pdfnvar(x1x2,m3,K3,2)
  }
}

pi1 <- length(ixg1) / (length(Clustx))
pi2 <- length(ixg2) / (length(Clustx))
pi3 <- length(ixg3) / (length(Clustx))

M12 <- pi2*M1 + pi2*M2 + pi3*M3

# persp3d(seqx1x2,seqx1x2,M1,col='red')
# persp3d(seqx1x2,seqx1x2,M2,col='blue',add=TRUE)
persp3d(seqx1x2,seqx1x2,M12,col='green',add=FALSE)

cat("\n===== Routine Finished=====\n")
