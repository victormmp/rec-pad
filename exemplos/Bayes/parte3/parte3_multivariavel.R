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

#======== ATE AQUI ESTA PRONTO

# ========== ESTIMANDO A FUNCAO GERADORA

# Media e desvio padrao da primeira feature da classe 1
m11 <- mean(xc1[,1])
sd11 <- sd(xc1[,1])

# Media e desvio padrao da segunda feature da classe 1
m12 <- mean(xc1[,2])
sd12 <- sd(xc1[,2])

# Media e desvio padrao da primeira feature da classe 2
m21 <- mean(xc2[,1])
sd21 <- sd(xc2[,1])

# Media e desvio padrao da segunda feature da classe 2
m22 <- mean(xc2[,2])
sd22 <- sd(xc2[,2])

# ========== ESTIMANDO AS PDFs MARGINAIS 

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
plot(y12range,xlim=c(0,10),xrange,ylim=c(0,10), col='red',type='l',xlab="",ylab="")
par(new=T)
plot(y22range,xlim=c(0,10),xrange,ylim=c(0,10), col='blue',type='l',xlab="",ylab="")

# ========== GERANDO GRAFICO 3D DAS PDFs DAS DISTRIBUICOES

y3d1 <- y11range %*% t(y12range)
y3d2 <- y21range %*% t(y22range)

#scatter3D(xc1[,1],xc1[,2],matrix(1,nrow=dim(xc1)[1],ncol=1))
# ribbon3D(xrange,xrange,y3d1)
# ribbon3D(xrange,xrange,y3d2,add=TRUE)

persp3D(xrange,xrange,y3d1)
persp3D(xrange,xrange,y3d2,add=TRUE)

# ========== CALCULO DO CLASSIFICADOR BINARIO DE BAYES
#
# Ja que os atributos de cada classe sao independentes, as verossimilhancas 
# posem ser calculadas diretamente por meio do produto das pdfs marginais.

pxc1 <- matrix(nrow=length(xrange),ncol=length(xrange))
pxc2 <- matrix(nrow=length(xrange),ncol=length(xrange))

# for(i in seq(length(xrange))) {
#   for(j in seq(length(xrange))) {
#     # P(x|C1)
#     pxc1[i,j] <- pcond2var(xrange[i],xrange[j],m21,m22,sd21,sd22)
#     
#     # P(x|C2)
#     pxc2[i,j] <- pcond2var(xrange[i],xrange[j],m11,m12,sd11,sd12)
#   }
# }

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