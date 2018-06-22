rm(list=ls())

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("tictoc")
library("class")
# library("stats")
library("corpcor")
library("vbmp")
source("functions.R")
library("caret")
library("naivebayes")
library("mlbench")
library("mclust")
library("hydroGOF")

cat("\014")
tic("Total elapsed time")

cat(">> Reading Data... ")
tic("Done")

########## CONSTANTS ##########

NUMBER_OF_SAMPLES <<- 500
TRAIN_SET_SIZE <<- 0.7

################################

# dataLocation <- ("parkinson/parkinsons.data")
# dataFiles <- read.table(dataLocation, sep = ",", header = TRUE)

dataLocation <- ("sapo/sapo.data")
dataFiles <- read.table(dataLocation,sep = ",", header = TRUE, stringsAsFactors = TRUE)

set.seed(101)
dataRow <- sample(nrow(dataFiles),NUMBER_OF_SAMPLES)
dataFiles <- dataFiles[dataRow,]

x <- dataFiles[,1:22]
y <- dataFiles[,23:ncol(dataFiles)]
toc()

results <- c()

########## REDUZINDO ATRIBUTOS COM PCA #########

cat(">> Reducing features with PCA... ")
tic("Done")

preProc <- preProcess(x,method="pca")
xPCA <- predict(preProc,x)
toc()

cat(">> Number of attributes for 95% accuracy: ", preProc$numComp," features.\n")

########## REDUZINDO ATRIBUTOS COM MDS
cat(">> Reducing features with MDS... ")
tic("Done")

dist_x <- dist(x)
xMDS <- cmdscale(dist_x,k = preProc$numComp)

toc()

########## DIVIDINDO AS CLASSES ##########

cat(">> Generating training and test datasets...")
tic("Done")
splitClasses()
toc()
########## CLASSIFICANDO COM KNN E PCA ##########

cat(">> Training with KNN and PCA... ")
tic("Done")

resultKNNPCA <- c()

for (i in seq(10)) {
    separation <- knn(xtreinoPCA,xtestePCA,ytreino[,3],k=i)
    resultKNNPCA <- rbind(resultKNNPCA,matrix(c(i,(checkAcc(separation, yteste[,3])[2])),ncol = 2))
}

colnames(resultKNNPCA) <- c("N","Accuracy")

meanResultKNNPCA <- mean(resultKNNPCA[,2])
results[["KNNPCA"]] <- resultKNNPCA

toc()

########## CLASSIFICANDO COM KNN E MDS ##########

cat(">> Training with KNN and MDS... ")
tic("Done")

resultKNNMDS <- c()

for (i in seq(10)) {
    separation <- knn(xtreinoMDS,xtesteMDS,ytreino[,3],k=i)
    resultKNNMDS <- rbind(resultKNNMDS,matrix(c(i,(checkAcc(separation, yteste[,3])[2])),ncol = 2))
}


colnames(resultKNNMDS) <- c("N","Accuracy")

meanResultKNNMDS <- mean(resultKNNMDS[,2])
results[["KNNMDS"]] <- resultKNNMDS

toc()

########## CLASSIFICANDO COM NAIVE BAYES E PCA ##########

cat(">> Training with Bayes and PCA... ")
tic("Done")

pred2 <- naive_bayes(x=xtreinoPCA, y=ytreino[,3], useKernel =TRUE)
predr <- predict(pred2,xtestePCA)
resultBayesPCA <- checkAcc(predr,yteste[,3])

toc()

########## CLASSIFICANDO COM NAIVE BAYES E MDS ##########

cat(">> Training with Bayes and MDS... ")
tic("Done")

pred3 <- naive_bayes(x=xtreinoMDS, y=ytreino[,3], useKernel =TRUE)
predr <- predict(pred3,xtesteMDS)
resultBayesMDS <- checkAcc(predr,yteste[,3])

toc()

###########################################

resultBayes <- rbind(resultBayesPCA,resultBayesMDS)

colnames(resultBayes) <- c("Success","Accuracy(%)")
rownames(resultBayes) <- c("PCA","MDS")

results[["Bayes"]] <- resultBayes

###########################################

########## MISTURA DE GAUSSIANAS ##########
cat(">> Initializing analyse through GMM and PCA\n")
tic("Finished GMM and PCA")

results[["GMMPCA"]] <- GMM(xPCA, y, "PCA")

toc()

cat(">> Initializing analyse through GMM and MDS\n")
tic("Finished GMM and MDS")

results[["GMMMDS"]] <- GMM(xMDS, y, "MDS")

toc()

plotBar(results)
plot(results$KNNPCA[,1],results$KNNPCA[,2],xlab="Valor de K", ylab="Precisão (%)",main="KNN e PCA")
plot(results$KNNMDS[,1],results$KNNMDS[,2],xlab="Valor de K", ylab="Precisão (%)",main="KNN e MDS")

toc()

trainY <- as.numeric(ytreino[,3])

SDPCA <- matrix(nrow=10, ncol = 12)
SDMDS <- matrix(nrow=10, ncol = 12)
rownames(SDPCA) <- c("Classe1", "Classe2","Classe3","Classe4","Classe5","Classe6","Classe7","Classe8","Classe9","Classe10")
rownames(SDMDS) <- c("Classe1", "Classe2","Classe3","Classe4","Classe5","Classe6","Classe7","Classe8","Classe9","Classe10")

colnames(SDPCA) <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12")
colnames(SDMDS) <- c("MDS1","MDS2","MDS3","MDS4","MDS5","MDS6","MDS7","MDS8","MDS9","MDS10","MDS11","MDS12")

MeanPCA <- matrix(nrow=10, ncol = 12)
MeanMDS <- matrix(nrow=10, ncol = 12)
rownames(MeanPCA) <- c("Classe1", "Classe2","Classe3","Classe4","Classe5","Classe6","Classe7","Classe8","Classe9","Classe10")
rownames(MeanMDS) <- c("Classe1", "Classe2","Classe3","Classe4","Classe5","Classe6","Classe7","Classe8","Classe9","Classe10")

colnames(MeanPCA) <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12")
colnames(MeanMDS) <- c("MDS1","MDS2","MDS3","MDS4","MDS5","MDS6","MDS7","MDS8","MDS9","MDS10","MDS11","MDS12")

for (j in 1:12) {
    for(i in 1:10) {
        
        SDPCA[i,j] <-sd(xtreinoPCA[which(trainY == i),j])
        SDMDS[i,j] <-sd(xtreinoMDS[which(trainY == i),j])
    }
   
}

for (j in 1:12) {
    for(i in 1:10) {
        
        MeanPCA[i,j] <-mean(xtreinoPCA[which(trainY == i),j])
        MeanMDS[i,j] <-mean(xtreinoMDS[which(trainY == i),j])
    }
    
}

print(t(MeanPCA))
print(t(MeanMDS))
print(t(SDPCA))
print(t(SDMDS))

#########################################################

trainY <- as.numeric(ytreino[,1])

SDPCA <- matrix(nrow=4, ncol = 12)
SDMDS <- matrix(nrow=4, ncol = 12)
rownames(SDPCA) <- c("Classe1", "Classe2","Classe3","Classe4")
rownames(SDMDS) <- c("Classe1", "Classe2","Classe3","Classe4")

colnames(SDPCA) <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12")
colnames(SDMDS) <- c("MDS1","MDS2","MDS3","MDS4","MDS5","MDS6","MDS7","MDS8","MDS9","MDS10","MDS11","MDS12")

MeanPCA <- matrix(nrow=4, ncol = 12)
MeanMDS <- matrix(nrow=4, ncol = 12)
rownames(MeanPCA) <- c("Classe1", "Classe2","Classe3","Classe4")
rownames(MeanMDS) <- c("Classe1", "Classe2","Classe3","Classe4")

colnames(MeanPCA) <- c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12")
colnames(MeanMDS) <- c("MDS1","MDS2","MDS3","MDS4","MDS5","MDS6","MDS7","MDS8","MDS9","MDS10","MDS11","MDS12")

for (j in 1:12) {
    for(i in 1:4) {
        
        SDPCA[i,j] <-sd(xtreinoPCA[which(trainY == i),j])
        SDMDS[i,j] <-sd(xtreinoMDS[which(trainY == i),j])
    }
    
}

for (j in 1:12) {
    for(i in 1:4) {
        
        MeanPCA[i,j] <-mean(xtreinoPCA[which(trainY == i),j])
        MeanMDS[i,j] <-mean(xtreinoMDS[which(trainY == i),j])
    }
    
}

print(t(MeanPCA))
print(t(MeanMDS))
print(t(SDPCA))
print(t(SDMDS))
