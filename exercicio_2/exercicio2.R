rm(list = ls())
graphics.off()

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

require(grDevices)
library('jpeg')
library('bmp')
library('pixmap')
library('ggplot2')

rotate <- function(x)
    t(apply(x, 2, rev))

#Leitura de dados ######################################################################
xt <- c()

xe <- c()


for (i in 1:5) {
    for (j in 1:5) {
        path_xt <-
            paste(getwd(), paste(
                paste("/faces/f", i, sep = ""),
                paste(paste("teste", j, sep = ""), ".bmp", sep = ""),
                sep = ""
            ) , sep = "")
        
        path_xe <-
            paste(getwd(), paste(
                paste("/faces/F", i, sep = ""),
                paste(paste("TESTE", j, sep = ""), "R.BMP", sep = ""),
                sep = ""
            ), sep = "")
        
        
        img_train <- read.bmp(path_xt)
        
        img_test <- read.bmp(path_xe)
        
        
        xt <- rbind(xt, t(as.matrix(as.vector(img_train))))
        xe <- rbind(xe, t(as.matrix(as.vector(img_test))))
    }
}

yt <-
    matrix(seq(1, 5, 1), ncol = 1, nrow = 25)
#yt = [1,2,3,4,5,1,2,3,4...];
ye <- matrix(seq(1, 5, 1), ncol = 1, nrow = 25)



sim <- function(a, b) {
    if (length(a) == length(b)) {
        size <- length(a)[1]
        ed <- 0
        for (i in seq(size)) {
            ed <- ed + (a[i] - b[i]) ** 2
        }
        ed <- sqrt(ed)
        return (ed)
    }
}

contaCor <- function(x) {
    x <- as.array(x)
    
    co <- array(0, 10)
    
    for (i in x) {
        if (i < 10)
            co[1] <- co[1] + 1
        else if (i < 20)
            co[2] <- co[2] + 1
        else if (i < 30)
            co[3] <- co[3] + 1
        else if (i < 40)
            co[4] <- co[4] + 1
        else if (i < 50)
            co[5] <- co[5] + 1
        else if (i < 60)
            co[6] <- co[6] + 1
        else if (i < 70)
            co[7] <- co[7] + 1
        else if (i < 80)
            co[8] <- co[8] + 1
        else if (i < 90)
            co[9] <- co[9] + 1
        else if (i < 100)
            co[10] <- co[10] + 1
    }
    return(co)
}

features1 <- function(x) {
    m <- mean(x)
    t <- sum(x)
    s <- sd(x)
    
    co <- contaCor(x)
    
    return(c(m, t, s, co))
}

features2 <- function(x) {
    d <- matrix(x, nrow = 56)
    v1 <- d[1:as.integer(dim(d)[1] / 2), ]
    v2 <- d[as.integer(dim(d)[1] / 2 + 1):dim(d)[1], ]
    
    v2 <- as.vector(v2)
    v1 <- as.vector(v1)
    
    f <- sim(v1, v2)
    m <- mean(x)
    t <- sum(x)
    s <- sd(x)
    
    return(c(f, m, t, s))
}

features3 <- function(x) {
    d <- matrix(x, nrow = 56)
    v1 <- d[1:as.integer(dim(d)[1] / 2), ]
    v2 <- d[as.integer(dim(d)[1] / 2 + 1):dim(d)[1], ]
    v3 <- d[, 1:as.integer(dim(d)[2] / 2)]
    v4 <- d[, as.integer(dim(d)[2] / 2 + 1):dim(d)[2]]
    
    v4 <- as.vector(v4)
    v3 <- as.vector(v3)
    v2 <- as.vector(v2)
    v1 <- as.vector(v1)
    
    f1 <- sim(v1, v2)
    f2 <- sim(v3, v4)
    f3 <- sim(v1, v3)
    f4 <- sim(v2, v4)
    f5 <- sim(v1, v4)
    f6 <- sim(v2, v3)
    
    return(c(f1, f2, f3, f4, f5, f6))
}


#=============================================

ft <- c()
xtf <- c()
xef <- c()

# Criando os vetores de feature para cada imagem dos conjuntos
for (i in seq(25)) {
    xtf <- rbind(xtf, t(as.matrix(features3(xt[i, ]))))
    xef <- rbind(xef, t(as.matrix(features3(xe[i, ]))))
}

# Inicia a comparação entre as imagens. Compara uma imagem de treinamento em xtf com uma
# imagem de teste em xef
for (i in seq(25)) {
    for (j in seq(25)) {
        ft <- c(ft, sim(xtf[i, ], xef[j, ]))
    }
}

# Cada coluna da matriz corresponde a uma imagem de treinamento
ft <- matrix(ft, nrow = 25, ncol = 25)


# Compara cada imagem de treinamento separadamente com cada imagem de teste
fr <- matrix(nrow = 5, ncol = 25)

for (i in seq(25)) {
    k <- 1
    for (j in seq(1, 21, 5)) {
        fr[k, i] <-
            which(ft[j:(j + 4), i] == min(ft[j:(j + 4), i]), arr.ind = TRUE)
        k <- k + 1
    }
}

# Obtém a quantidade de acertos
acertos <- 0
for (i in seq(5)) {
    for (j in seq(25)) {
        if (fr[i, j] == ye[j])
            acertos <- acertos + 1
    }
}


print(c("Acertos: ", acertos))
print(c("Acertos em %:", acertos / (25 * 5) * 100))
