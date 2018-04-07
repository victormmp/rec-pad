rm(list=ls())

# install.packages("plot3D")
# install.packages("rgl")
library("plot3D")
library("rgl")

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# ========== FUNÇÕES


fnormal1var <- function(x,m,r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))**2)

  return(y)
}

pcond2var <- function(x1,x2,m1,m2,r1,r2) {
  y <- (1/(sqrt(2*pi*r1*r1*r2*r2)))*exp(-(0.5*((x1-m1)/(r1))**2 + 0.5*((x2-m2)/(r2))**2))
  
  return(y)
}

# ========== CRIAÇÃO DAS AMOSTRAS

# Número de Parâmetros
par <- 2
#Número de Amostras
N <- 60

xc1 <- matrix(rnorm(N*par, mean=3,sd=1),ncol=par)
xc2 <- matrix(rnorm(N*par, mean=7,sd=1),ncol=par)

# ========== PLOTAGEM DAS AMOSTRAS

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

# ========== ESTIMANDO A FUNÇÃO GERADORA

# Média e desvio padrão da primeira feature da classe 1
m11 <- mean(xc1[,1])
sd11 <- sd(xc1[,1])

# Média e desvio padrão da segunda feature da classe 1
m12 <- mean(xc1[,2])
sd12 <- sd(xc1[,2])

# Média e desvio padrão da primeira feature da classe 2
m21 <- mean(xc2[,1])
sd21 <- sd(xc2[,1])

# Média e desvio padrão da segunda feature da classe 2
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

# ========== GERANDO GRÁFICO 3D DAS PDFs DAS DISTRIBUIÇÕES

y3d1 <- y11range %*% t(y12range)
y3d2 <- y21range %*% t(y22range)

#scatter3D(xc1[,1],xc1[,2],matrix(1,nrow=dim(xc1)[1],ncol=1))
# ribbon3D(xrange,xrange,y3d1)
# ribbon3D(xrange,xrange,y3d2,add=TRUE)

persp3d(xrange,xrange,y3d1,col='red')
persp3d(xrange,xrange,y3d2,col='blue',add=TRUE)

# ========== CÁLCULO DO CLASSIFICADOR BINÁRIO DE BAYES
#
# Já que os atributos de cada classe são independentes, as verossimilhanças 
# posem ser calculadas diretamente por meio do produto das pdfs marginais.


# P(x|C1)
pxc1 <- y3d1

# P(x|C2)
pxc2 <- y3d2

# P(C1)
pc1 <- dim(xc1)[1]/(dim(xc1)[1] + dim(xc2)[1])
# P(C2)
pc2 <- dim(xc2)[1]/(dim(xc1)[1] + dim(xc2)[1])

# Classificador
# 
# Classe(x) = C1 se P(x|C1)/P(x|C2) > P(C2)/P(C1)
#           = C2 caso contrário

yclass <- matrix(1, nrow = length(xrange), ncol = length(xrange))

for (i in seq(length(xrange))) {
  for (j in seq(length(xrange))) {
    yclass[i,j] <- (max(y3d1)/2) * (pxc1[i,j]/pxc2[i,j] > pc2/pc1)
  }
}

# Incluindo o separador no plot 3d das pdfs das distribuições
persp3d(xrange,xrange,yclass,col='green',add=TRUE)









