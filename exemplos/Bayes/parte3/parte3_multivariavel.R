rm(list=ls())
# install.packages("plot3D")
# install.packages("rgl")
# install.packages("bmp")
library("plot3D")
library("rgl")
library('bmp')

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# ========== FUNCOES

source("Features.R")
source("distProb.R")
source("ReadFaces.R")

# ========== CRIACAO DAS AMOSTRAS

x <- readFaces(this.dir)

xt <- x[1:25,]
xe <- x[26:50,]

xtf <- c()
for (i in seq(25)) {
  xtf <- rbind(xtf, t(as.matrix(features3(xt[i,]))))
}

c1 <- xtf[1:5,]
c2 <- xtf[6:10,]
c3 <- xtf[11:15,]
c4 <- xtf[16:20,]
c5 <- xtf[21:25,]

#======== Montando as matrizes de covariâncias

mc1 <- matrix(nrow=dim(c1)[2]);
mc2 <- matrix(nrow=dim(c2)[2]);
mc3 <- matrix(nrow=dim(c3)[2]);
mc4 <- matrix(nrow=dim(c4)[2]);
mc5 <- matrix(nrow=dim(c5)[2]);

sdc1 <- matrix(nrow=dim(c1)[2]);
sdc2 <- matrix(nrow=dim(c2)[2]);
sdc3 <- matrix(nrow=dim(c3)[2]);
sdc4 <- matrix(nrow=dim(c4)[2]);
sdc5 <- matrix(nrow=dim(c5)[2]);

p1 <- cor(c1)
p2 <- cor(c2)
p3 <- cor(c3)
p4 <- cor(c4)
p5 <- cor(c5)

for (i in 1:6) {
    mc1[i] <- mean(c1[,i]);
    mc2[i] <- mean(c2[,i]);
    mc3[i] <- mean(c3[,i]);
    mc4[i] <- mean(c4[,i]);
    mc5[i] <- mean(c5[,i]);
    
    sdc1[i] <- sd(c1[,i]);
    sdc2[i] <- sd(c2[,i]);
    sdc3[i] <- sd(c3[,i]);
    sdc4[i] <- sd(c4[,i]);
    sdc5[i] <- sd(c5[,i]);
}

sumc1 <- sdc1 %*% t(sdc1) * t(p1)
sumc2 <- sdc2 %*% t(sdc2) * t(p2)
sumc3 <- sdc3 %*% t(sdc3) * t(p3)
sumc4 <- sdc4 %*% t(sdc4) * t(p4)
sumc5 <- sdc5 %*% t(sdc5) * t(p5)

# ======== ATÉ AQUI TÁ PRONTO

for(i in seq(length(xrange))) {
  for(j in seq(length(xrange))) {
    # P(x|C1)
    pxc1[i,j] <- pcond2varcorr(xrange[i],xrange[j],m21,m22,sd21,sd22,p1)
    
    # P(x|C2)
    pxc2[i,j] <- pcond2varcorr(xrange[i],xrange[j],m11,m12,sd11,sd12,p2)
  }
}

# P(C1)
pc1 <- dim(xc1)[1]/(dim(xc1)[1] + dim(xc2)[1])
# P(C2)
pc2 <- dim(xc2)[1]/(dim(xc1)[1] + dim(xc2)[1])

# Classificador
# 
# Classe(x) = C1 se P(x|C1)/P(x|C2) > P(C2)/P(C1)
#           = C2 caso contrario

yclass <- matrix(1, nrow = length(xrange), ncol = length(xrange))

for (i in seq(length(xrange))) {
  for (j in seq(length(xrange))) {
    yclass[i,j] <- (max(y3d1)/2) * (pxc1[i,j]/pxc2[i,j] > pc2/pc1)
  }
}

# Incluindo o separador no plot 3d das pdfs das distribuicoes
persp3D(xrange,xrange,yclass,add=TRUE)