rm(list = ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

cat("\f")

cat("===== Starting Routine =====\n")

require(RnavGraphImageData)
# install.packages("class")
# install.packages("stats")
# source("http://bioconductor.org/biocLite.R") 
# biocLite("vbmp")
library("class")
library("stats")
library("tictoc")
library("corpcor")
library(vbmp)

tic("Total elapsed time")

# Carregando a Base de dados
cat(">> Loading database...\n")

data( faces )
faces <- t( faces )
rotate <- function(x) t( apply(x, 2, rev) )

cat(">> Importing methods...\n")

source("functionsImagem.R")
source("checkAccFunction.R")

#Gerando os rotulos
cat(">> Generating data labels...")

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

rm(nomeColunas)
rm(nomeLinhas)

cat(" Done!\n")

# ===== DIMINUINDO O NÚMERO DE ATRIBUTOS COM PCA =====

cat(">> Decreasing number of features with PCA... ")

results <- list()

tic("Done")

facesPCAaux <- prcomp(faces, center=TRUE, retx=TRUE, scale=TRUE)
facesPCA <- facesPCAaux$x[,1:120]

toc()

# ===== DIMINUINDO O NÚMERO DE ATRIBUTOS COM MDS =====

cat(">> Decreasing number of features with MDS... ")
tic("Done")

kMDS <- 120

facesMDS <- cmdscale(dist(faces), k=kMDS)
toc()

cat(c("\nNumber of features for MDS reduction: ", kMDS, " features.\n"))
cat("\n")


# ===== GERANDO SETS DE TREINO E TESTE =====

# Get random for tests
cat(">> Creating random training and test datasets...")
tic("Done")

dim_classe <- 10
numClasses <- 400
numAmostras <- 10

seqN <- sample(numAmostras)
porcAmostTrain <- 0.5

N <- seqN[1:(porcAmostTrain*numAmostras)]
nSamplesTrain <- length(N)
n <- seqN[(porcAmostTrain*numAmostras+1):numAmostras]
nSamplesTest <- length(n)

xtreino <- c()
xtreinoPCA <- c()
xtreinoMDS <- c()
ytreino <- c()
xteste <- c()
xtestePCA <- c()
xtesteMDS <- c()
yteste <- c()

for(r in seq(1,numClasses,numAmostras)) {
    for(i in N) {
        xtreino <- rbind(xtreino, (faces[r+i-1,]))
        xtreinoPCA <- rbind(xtreinoPCA, (facesPCA[r+i-1,]))
        xtreinoMDS <- rbind(xtreinoMDS, (facesMDS[r+i-1,]))
        ytreino <- c(ytreino,(y[r+i-1]))
    }
    
    for(i in n) {
        xteste <- rbind(xteste, (faces[r+i-1,]))
        xtestePCA <- rbind(xtestePCA, (facesPCA[r+i-1,]))
        xtesteMDS <- rbind(xtesteMDS, (facesMDS[r+i-1,]))
        yteste <- c(yteste,(y[r+i-1]))
    }
}

toc()

# ============ Classificando com KNN

cat(">> Training with KNN and PCA... ")
tic("Done")

resultKNNPCA <- c()


for (i in seq(10)) {
    separation <- knn(xtreinoPCA,xtestePCA,ytreino,k=i)
    resultKNNPCA <- rbind(resultKNNPCA,matrix(c(i,(checkAcc(separation, yteste)[2])),ncol = 2))
}


colnames(resultKNNPCA) <- c("N","Accuracy")

toc()

meanResultKNNPCA <- mean(resultKNNPCA[,2])
results[["KNNPCA"]] <- resultKNNPCA


cat("\nAccuracy of KNN and PCA for diferents N neighbours")
cat("\n==================================================")
cat("\n   N        Acc (%)  \n")
for(i in seq(10)){
    cat(c("\n  ",i, "     ", resultKNNPCA[i,2], "  "))
}
cat("\n")

cat(">> Training with KNN and MDS... ")
tic("Done")

resultKNNMDS <- c()

for (i in seq(10)) {
    separation <- knn(xtreinoMDS,xtesteMDS,ytreino,k=i)
    resultKNNMDS <- rbind(resultKNNMDS,matrix(c(i,(checkAcc(separation, yteste)[2])),ncol = 2))
}


colnames(resultKNNMDS) <- c("N","Accuracy")

toc()

meanResultKNNMDS <- mean(resultKNNMDS[,2])
results[["KNNMDS"]] <- resultKNNMDS

cat("\nAccuracy of KNN and MDS for diferents N neighbours")
cat("\n==================================================")
cat("\n   N        Acc(%)  \n")
for(i in seq(10)){
    cat(c("\n  ",i, "     ", resultKNNMDS[i,2], "  "))
}
cat("\n")

cat(">> Training with Bayes and PCA... ")
tic("Done")

predPCA <- bayes(xtreinoPCA, ytreino, nSamplesTest, xtestePCA, yteste)

results[["BayesPCA"]] <- predPCA

toc()

cat(">> Training with Bayes and MDS... ")
tic("Done")

predMDS <- bayes(xtreinoMDS, ytreino, nSamplesTest, xtesteMDS, yteste)

results[["BayesMDS"]] <- predMDS

toc()

cat("\n===== Routine Finished =====\n")
toc()