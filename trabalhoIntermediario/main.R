rm(list = ls())

cat("\f")

cat("===== Starting Routine =====\n")

require(RnavGraphImageData)
# install.packages("class")
# install.packages("stats")
library("class")
library("stats")
library("caret")
# library("tictoc")

# Carregando a Base de dados
cat(">> Carregando a base de dados...\n")

data( faces )
faces <- t( faces )
rotate <- function(x) t( apply(x, 2, rev) )

cat(">> Importando funÃ§Ãµes...\n")

source("functionsImagem.R")


#Gerando os rotulos
cat(">> Gerando os rorulos para os dados...")

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

cat(" Rotulos gerados.\n")
# =========================

# Get random for tests
cat(">> Gerando dados de treino e teste aleatorios...")

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

cat(" Dados gerados.\n")

# cat(c("Dim xtreino: ",dim(xtreino)))


#==============================

# Library KNN

# Funcao PCA
# prcomp(xtreino, scale=TRUE, retx=TRUE)

# preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
# spamPC <- predict(preProc,log10(spam[,-58]+1))
# plot(spamPC[,1],spamPC[,2],col=typeColor)

# Funcao MDS
# cmdscale( xtreino, k=2)

# Funcao KNN
# knn(xtreino,xteste,ytreino,k=3)

# funcao Bayes
# 

# Achando o menor numero de atributos pelo pca

cat(">> Inicializando treinamento...\n")

exat <- 0.9

# xtreinoPCA <- prcomp(xtreino, scale=TRUE)
xtreinoMDS <- cmdscale(dist(xtreino), k=2)

preProc <- preProcess(xtreino,method="pca",pcaComp=3)
spamPC <- predict(preProc,xteste)
plot(spamPC[,1],spamPC[,2])
