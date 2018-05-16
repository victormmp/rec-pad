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

for(SD in c(0.3, 0.5, 0.7)){
    for (k in c(2,4,8)) {
        
        nSamples <- 100
        # Number of clusters
        # k <- 2
        # Max Iteration
        maxit <- 100
        
        xg1 <- matrix(rnorm(nSamples,mean=2,sd=SD),ncol=2)
        xg2 <- matrix(rnorm(nSamples,mean=4,sd=SD),ncol=2)
        xg3 <- matrix(rnorm(nSamples,mean=0,sd=SD),ncol=2)
        xg4 <- matrix(rnorm(nSamples,mean=-2,sd=SD),ncol=2)
        
        X <- rbind(xg1,xg2,xg3,xg4)
        
        plotLim <- c(-10,10)
        
        plot(xg1[,1],xg1[,2],xlim=plotLim,ylim=plotLim)
        par(new=T)
        plot(xg2[,1],xg2[,2],xlim=plotLim,ylim=plotLim)
        par(new=T)
        plot(xg3[,1],xg3[,2],xlim=plotLim,ylim=plotLim)
        par(new=T)
        plot(xg4[,1],xg4[,2],xlim=plotLim,ylim=plotLim)
        
        cat(">> Calculatint centers\n")
        McList <- kMeans(X,k,maxit)
        Mc <- McList$Mc
        Clustx <- McList$Clustx
        
        xCluster1 <- list()
        
        for(i in seq(k)) {
            ici <- which(Clustx == i)
            xCluster1[[i]] <- ici
        }
        
        colnames(Mc) <- c("x1","x2")
        
        linhaNome <- c()
        for(i in seq(k)) {
            linhaNome <- c(linhaNome, paste("centro ", i, ":"))
        }
        
        rownames(Mc) <- linhaNome
        
        print(Mc)
        
        fileName <- paste("k", k, "sd", SD, ".png", sep = "")
        png(filename=fileName)
        
        plot(X[xCluster1[[1]],1],X[xCluster1[[1]],2],xlim=plotLim,ylim=plotLim,col=1, xlab="x1", ylab="x2")
        par(new=T)
        plot(X[xCluster1[[2]],1],X[xCluster1[[2]],2],xlim=plotLim,ylim=plotLim,col=2, xlab="", ylab="")
        par(new=T)
        plot(Mc[1,1],Mc[1,2],xlim=plotLim,ylim=plotLim,col=1,pch=15, xlab="", ylab="")
        par(new=T)
        plot(Mc[2,1],Mc[2,2],xlim=plotLim,ylim=plotLim,col=4, pch=15, xlab="", ylab="")
        
        if(k>=3) {
            par(new=T)
            plot(X[xCluster1[[3]],1],X[xCluster1[[3]],2],xlim=plotLim,ylim=plotLim,col=3, xlab="", ylab="")
            par(new=T)
            plot(Mc[3,1],Mc[3,2],xlim=plotLim,ylim=plotLim,col=4, pch=15, xlab="", ylab="")
        } 
        if (k>=4) {
            par(new=T)
            plot(X[xCluster1[[4]],1],X[xCluster1[[4]],2],xlim=plotLim,ylim=plotLim,col=4, xlab="", ylab="")
            par(new=T)
            plot(Mc[4,1],Mc[4,2],xlim=plotLim,ylim=plotLim,col=4, pch=15, xlab="", ylab="")
        }
        
        title(paste("Clusters para k = ", k, " e sd = ", SD))
        dev.off()
        
    }
}



# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

###########

cat(">> Generating 3D plot\n")

seqx1x2 <- seq(-10,10,0.1)
lseq <- length(seqx1x2)
M1 <- matrix(nrow=lseq,ncol=lseq)
m1 <- colMeans(X[xCluster1[[1]],])
K1 <- cov(X[xCluster1[[1]],])


M2 <- matrix(nrow=lseq,ncol=lseq)
m2 <- colMeans(X[xCluster1[[2]],])
K2 <- cov(X[xCluster1[[2]],])

if (k>=3) {
    M3 <- matrix(nrow=lseq,ncol=lseq)
    m3 <- colMeans(X[xCluster1[[3]],])
    K3 <- cov(X[xCluster1[[3]],])
} else {
    M3 <- 0
}

if(k>=4) {
    M4 <- matrix(nrow=lseq,ncol=lseq)
    m4 <- colMeans(X[xCluster1[[4]],])
    K4 <- cov(X[xCluster1[[4]],])
} else {
    M4 <- 0
}

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
        if(k>=3) M3[i,j] <- pdfnvar(x1x2,m3,K3,2)
        if(k>=4) M4[i,j] <- pdfnvar(x1x2,m4,K4,2)
    }
}

pi1 <- length(xCluster1[[1]]) / (length(Clustx))
pi2 <- length(xCluster1[[2]]) / (length(Clustx))
if(k>=3) {pi3 <- length(xCluster1[[3]]) / (length(Clustx))} else pi3 <- 0
if(k>=4) {pi4 <- length(xCluster1[[4]]) / (length(Clustx))} else pi4 <- 0

M12 <- pi2*M1 + pi2*M2 + pi3*M3 + pi4*M4

# persp3d(seqx1x2,seqx1x2,M1,col='red')
# persp3d(seqx1x2,seqx1x2,M2,col='blue',add=TRUE)
persp3d(seqx1x2,seqx1x2,M12,col='green',add=FALSE)

cat("\n===== Routine Finished=====\n")
