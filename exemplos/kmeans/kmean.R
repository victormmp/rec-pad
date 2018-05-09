rm(list=ls())

cat("\014")
# library("tictoc")

cat("===== Starting Routine=====\n\n")

cat(">> Creating kMeans function\n")

kMeans <- function (X, k, maxit) {
  N <- dim(X)[1]
  n <- dim(X)[2]
  
  seqN <- seq(1,N,1)
  seqk <- seq(1,k,1)
  
  Mc <- matrix(nrow=k, ncol=n)
  Clustx <-matrix(nrow=N, ncol=1)
  
  seqx <- sample(N,2)
  
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

cat(">> Creating samples\n")

n1 <- 60
n2 <- 60

xg1 <- matrix(rnorm(n1,mean=2,sd=1),ncol=2)
xg2 <- matrix(rnorm(n2,mean=4,sd=1),ncol=2)

plot(xg1[,1],xg1[,2],xlim=c(0,10),ylim=c(0,10))
par(new=T)
plot(xg2[,1],xg2[,2],xlim=c(0,10),ylim=c(0,10))


cat(">> Calculating distance matrix\n")

k <- 2
X <- rbind(xg1,xg2)
maxit <- 100

cat(">> Calculatint centers\n")
McList <- kMeans(X,k,maxit)
Mc <- McList$Mc
Clustx <- McList$Clustx

ixg1 <- which(Clustx == 1)
ixg2 <- which(Clustx == 2)

print(Mc)


plot(Mc[1,1],Mc[1,2],xlim=c(0,10),ylim=c(0,10),col=1,pch=15)
par(new=T)
plot(Mc[2,1],Mc[2,2],xlim=c(0,10),ylim=c(0,10),col=4, pch=15)

# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

par(new=T)
plot(X[ixg1,1],X[ixg1,2],xlim=c(0,10),ylim=c(0,10),col=2)
par(new=T)
plot(X[ixg2,1],X[ixg2,2],xlim=c(0,10),ylim=c(0,10),col=3)

cat("\n===== Routine Finished=====\n")
