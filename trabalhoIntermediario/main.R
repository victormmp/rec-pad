rm(list = ls())
require(RnavGraphImageData)
# Carregando a Base de dados
data( faces )
faces <- t( faces )
rotate <- function(x) t( apply(x, 2, rev) )

source("functionsImagem.R")

cat("===== Starting Routine =====")

#Gerando os rotulos
y <- NULL
for(i in 1:nrow(faces) )
{
  y <- c( y, ((i-1) %/% 10) + 1 )
}

# Nomeando os atributos
nomeColunas <- NULL
for(i in 1:ncol(faces) )
{
  nomeColunas <- c(nomeColunas, paste("a", as.character(i), sep=".") )
}

nomeLinhas <- NULL
for(i in 1:nrow(faces)) {
    nomeLinhas <- c(nomeLinhas, paste("face", as.character(y[i]),as.character(i),sep="."))
}

colnames(faces) <- nomeColunas

faces <- as.data.frame(faces, row.names = nomeLinhas)

# rownames(faces) <- nomeLinhas

rm(nomeColunas)
rm(nomeLinhas)
# =========================

# Get random for tests

dim_classe <- 10

N <- sample(dim_classe, 0.7 * dim_classe)
n <- sample(dim_classe, 0.3 * dim_classe)

numClasses <- 400
numAmostras <- 10

xtreino <- c()
ytreino <- c()
xteste <- c()
yteste <- c()

for(r in seq(1,numClasses,numAmostras)) {
  for(i in N) {
    xtreino <- rbind(xtreino, (faces[r+i-1,]))
    ytreino <- c(ytreino,(y[r+i-1]))
  }
  
  for(i in n) {
    xteste <- rbind(xteste, (faces[r+i-1,]))
    yteste <- c(yteste,(y[r+i-1]))
  }
}

cat(c("Dim xtreino: ",dim(xtreino)))


#==============================

# Library KNN

# install.packages("class")
# install.packages("stats")
library("class")
library("stats")

# Funcao PCA
# prcomp(xtreino, scale=TRUE, retx=TRUE)

# Funcao MDS
# cmdscale( xtreino, k=2)

# Funcao KNN
# knn(xtreino,xteste,ytreino,k=3)

# funcao Bayes
# 

# Achando o menor numero de atributos pelo pca

exat <- 0.9

xtreinoPCA <- prcomp(xtreino, scale=TRUE)
xtreinoMDS <- cmdscale(dist(xtreino), k=2)





