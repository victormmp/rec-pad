rm(list=ls())

source("pxkde1var.R")
library("ggplot2")

x1 <- rnorm(100,mean=2,sd=0.2)
x2 <- rnorm(100, mean=4, sd=0.2)

x <- c(x1,x2)

# Para uma variavel

xrange <- seq(0,6,0.001)

yzero <- rep(0,length(x1))

px <- pxkde1var(xrange,x1)

limiteY <- c(0,2)
limiteX <- c(0,5)

pxCerto <- fnormal1var(xrange,2,0.2)
par(mfrow=c(1,3))

plot(x1,yzero,ylim=limiteY, xlim=limiteX)
par(new=T)
plot(xrange,px,type="l",ylim=limiteY,xlim=limiteX)
par(new=T)
plot(xrange,pxCerto,type="l",ylim=limiteY,xlim=limiteX, col="red")

# Para duas variaveis

yzero <- rep(0,length(x))

px <- pxkde1var(xrange,x)

limiteY <- c(0,1)
limiteX <- c(0,5)

pxCerto <- fnormal1var(xrange,mean(x),sd(x))

plot(x,yzero,ylim=limiteY, xlim=limiteX)
par(new=T)
plot(xrange,px,type="l",ylim=limiteY,xlim=limiteX)
par(new=T)
plot(xrange,pxCerto,type="l",ylim=limiteY,xlim=limiteX, col="red")

# Para uma variavel de dois parametros

yzero <- rep(0,length(x1))

px1 <- pxkde1var(xrange,x1)
px2 <- pxkde1var(xrange,x2)

M <- matrix(nrow=length(xrange), ncol=length(xrange))

for(i in 1:length(xrange)) {
  for (j in 1:length(xrange)) {
    M <- px1[i] * px1[j]
  }
}

limiteY <- c(0,1)
limiteX <- c(0,5)

# pxCerto <- fnormal1var(xrange,mean(x),sd(x))

plot3d(xrange,xrage,M)
