rm(list = ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

cat("\f")

cat("===== Starting Routine =====\n")
tic("Total elapsed time")

require(RnavGraphImageData)
# install.packages("class")
# install.packages("stats")
library("class")
library("stats")
library("caret")
library("tictoc")

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

# ======================================

cat(">> Decreasing number of features with PCA... ")
tic("Done")

exat <- 0.9

preProc <- preProcess(faces,method="pca")
facesPCA <- predict(preProc,faces)
toc()

cat(c("\nMinimum number of features for 95% accuracy with PCA: ", preProc$numComp, " features.\n"))
cat("\n")

cat(">> Decreasing number of features with MDS... ")
tic("Done")

facesMDS <- cmdscale(dist(faces), k=preProc$numComp)
toc()

# plot(1:4096,facesPCA$std)


# Get random for tests
cat(">> Creating random training and test datasets...")
tic("Done")

dim_classe <- 10
numClasses <- 400
numAmostras <- 10

seqN <- sample(numAmostras)
porcAmostTrain <- 0.3

N <- seqN[1:(porcAmostTrain*numAmostras)]
n <- seqN[(porcAmostTrain*numAmostras+1):numAmostras]

xtreinoPCA <- c()
xtreinoMDS <- c()
ytreino <- c()
xtestePCA <- c()
xtesteMDS <- c()
yteste <- c()

for(r in seq(1,numClasses,numAmostras)) {
  for(i in N) {
    xtreinoPCA <- rbind(xtreinoPCA, (facesPCA[r+i-1,]))
    xtreinoMDS <- rbind(xtreinoMDS, (facesMDS[r+i-1,]))
    ytreino <- c(ytreino,(y[r+i-1]))
  }
  
  for(i in n) {
    xtestePCA <- rbind(xtestePCA, (facesPCA[r+i-1,]))
    xtesteMDS <- rbind(xtesteMDS, (facesMDS[r+i-1,]))
    yteste <- c(yteste,(y[r+i-1]))
  }
}

toc()

# ============ Classificando com KNN

cat(">> Training with KNN and PCA... ")
tic("Done")

resultKNN <- c()

for (i in seq(10)) {
    separation <- knn(xtreinoPCA,xtestePCA,ytreino,k=i)
    resultKNN <- c(resultKNN, (checkAcc(separation, yteste)[2]))
}

toc()

meanResultKNN <- mean(resultKNN)

cat("\nAccuracy of KNN and PCA for diferents N neighbours")
cat("\n==================================================")
cat("\n   N        Acc (%)  \n")
for(i in seq(10)){
    cat(c("\n  ",i, "     ", resultKNN[i], "  "))
}
cat("\n")

cat(">> Training with KNN and MDS... ")
tic("Done")

resultKNN <- c()

for (i in seq(10)) {
    separation <- knn(xtreinoMDS,xtesteMDS,ytreino,k=i)
    resultKNN <- c(resultKNN, (checkAcc(separation, yteste)[2]))
}

toc()

meanResultKNN <- mean(resultKNN)

cat("\nAccuracy of KNN and MDS for diferents N neighbours")
cat("\n==================================================")
cat("\n   N        Acc(%)  \n")
for(i in seq(10)){
    cat(c("\n  ",i, "     ", resultKNN[i], "  "))
}
cat("\n")

#==============================

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

# spamPC <- predict(preProc,xteste)
# plot(spamPC[,1],spamPC[,2])

cat("\n===== Routine Finished =====\n")
toc()