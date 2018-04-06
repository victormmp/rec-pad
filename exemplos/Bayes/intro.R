rm(list=ls())

# install.packages("plot3D")
# install.packages("rgl")
library("plot3D")
library("rgl")


fnormal1var <- function(x,m,r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))**2)

  return(y)
}

pcond2var <- function(x1,x2,m1,m2,r1,r2) {
  y <- (1/(sqrt(2*pi*r1*r1*r2*r2)))*exp(-(0.5*((x1-m1)/(r1))**2 + 0.5*((x2-m2)/(r2))**2))
  
  return(y)
}


# Número de Parâmetros
par <- 2
#Número de Amostras
N <- 60

xc1 <- matrix(rnorm(N*par, mean=3,sd=1),ncol=par)
xc2 <- matrix(rnorm(N*par, mean=7,sd=1),ncol=par)



plot(xc1[,1],xc1[,2],xlim=c(0,10),ylim=c(0,10), col='red',xlab="Parametro 1 da classe",ylab="Parametro 2 da classe")
par(new=T)
plot(xc2[,1],xc2[,2],xlim=c(0,10),ylim=c(0,10), col='blue',xlab="",ylab="")
par(new=T)
plot(xc1[,1],matrix(0,nrow=length(xc1[,1]),ncol=1),xlim=c(0,10),ylim=c(0,10), col='red',xlab="",ylab="")
par(new=T)
plot(xc2[,1],matrix(0,nrow=length(xc2[,1]),ncol=1),xlim=c(0,10),ylim=c(0,10), col='blue',xlab="",ylab="")
par(new=T)
plot(matrix(0,nrow=length(xc1[,2]),ncol=1),xc1[,1],xlim=c(0,10),ylim=c(0,10), col='red',xlab="",ylab="")
par(new=T)
plot(matrix(0,nrow=length(xc2[,2]),ncol=1),xc2[,1],xlim=c(0,10),ylim=c(0,10), col='blue',xlab="",ylab="")

## estimando função geradora
m11 <- mean(xc1[,1])
sd11 <- sd(xc1[,1])

m12 <- mean(xc1[,2])
sd12 <- sd(xc1[,2])


m21 <- mean(xc2[,1])
sd21 <- sd(xc2[,1])

m22 <- mean(xc2[,2])
sd22 <- sd(xc2[,2])

## Adiciona Densidades ao gráfico

xrange <- seq(0,10,0.1)
y11range <- fnormal1var(xrange,m11,sd11)
y12range <- fnormal1var(xrange,m12,sd12)
y21range <- as.matrix(fnormal1var(xrange,m21,sd21))
y22range <- as.matrix(fnormal1var(xrange,m22,sd22))

par(new=T)
plot(xrange,y11range,xlim=c(0,10),ylim=c(0,10), col='red',type='l',xlab="",ylab="")
par(new=T)
plot(xrange,y21range,xlim=c(0,10),ylim=c(0,10), col='blue',type='l',xlab="",ylab="")
par(new=T)
plot(y11range,xlim=c(0,10),xrange,ylim=c(0,10), col='red',type='l',xlab="",ylab="")
par(new=T)
plot(y21range,xlim=c(0,10),xrange,ylim=c(0,10), col='blue',type='l',xlab="",ylab="")

## Gerando graf 3D

y3d1 <- y11range %*% t(y12range)
y3d2 <- y21range %*% t(y22range)

#scatter3D(xc1[,1],xc1[,2],matrix(1,nrow=dim(xc1)[1],ncol=1))
# ribbon3D(xrange,xrange,y3d1)
# ribbon3D(xrange,xrange,y3d2,add=TRUE)

persp3d(xrange,xrange,y3d1,col='red')
persp3d(xrange,xrange,y3d2,col='blue',add=TRUE)

# P(x|C1)
px1c1 <- pcond2var(xc1[,1],xc1[,2],m11,m12,sd11,sd12)
px2c1 <- pcond2var(xc2[,1],xc2[,2],m21,m22,sd21,sd22)

# P(x|C2)
px1c2 <- pcond2var(xc1[,1],xc1[,2],m11,m12,sd11,sd12)
px2c2 <- pcond2var(xc2[,1],xc2[,2],m21,m22,sd21,sd22)

pxc2 <- px1c1 %*% t(px1c2)
pxc1 <- px2c1 %*% t(px2c2)

# P(C1)
pc1 <- dim(xc1)[1]/(dim(xc1)[1] + dim(xc2)[1])
pc2 <- dim(xc2)[1]/(dim(xc1)[1] + dim(xc2)[1])

# Classificador

yclass <- matrix(0, nrow = length(xrange), ncol = length(xrange))

for (i in xrange) {
  for (j in xrange) {
    yclass[i,j] <- 0.05 * (pxc1[i,j]/pxc2[i,j] >= pc2/pc1)
  }
}

persp3d(xrange,xrange,yclass,col='green',add=TRUE)









