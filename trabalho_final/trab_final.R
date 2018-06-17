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

results[["resultsBayes"]] <- resultBayes

###########################################

########## MISTURA DE GAUSSIANAS ##########
cat(">> Initializing analyse through GMM\n")
tic("Finished GMM")

cat(">>>> Generating indexes... ")
trainY <- as.numeric(y[,1])

indexC1 <- which(trainY == 1)
indexC2 <- which(trainY == 2)
indexC3 <- which(trainY == 3)
indexC4 <- which(trainY == 4)

index1 <- sample(length(indexC1))
index2 <- sample(length(indexC2))
index3 <- sample(length(indexC3))
index4 <- sample(length(indexC4))

N <- length(index1)

toc()

cat(">>>> Generating training and test datasets... ")
tic("Done")

# xPCA <- as.matrix(xPCA)

trainX1 <- xPCA[indexC1[index1[1:(TRAIN_SET_SIZE*length(index1))]],]
testX1 <- xPCA[indexC1[index1[(TRAIN_SET_SIZE*length(index1) + 1):length(index1)]],]

trainX2 <- xPCA[indexC2[index2[1:(TRAIN_SET_SIZE*length(index2))]],]
testX2 <- xPCA[indexC2[index2[(TRAIN_SET_SIZE*length(index2) + 1):length(index2)]],]

trainX3 <- xPCA[indexC3[index3[1:(TRAIN_SET_SIZE*length(index3))]],]
testX3 <- xPCA[indexC3[index3[(TRAIN_SET_SIZE*length(index3) + 1):length(index3)]],]

trainX4 <- xPCA[indexC4[index4[1:(TRAIN_SET_SIZE*length(index4))]],]
testX4 <- xPCA[indexC4[index4[(TRAIN_SET_SIZE*length(index4) + 1):length(index4)]],]

testX <- rbind(testX1, testX2, testX3, testX4)
testY <- c(rep(1, dim(testX1)[1]),rep(2,dim(testX2)[1]),rep(3, dim(testX3)[1]),rep(4, dim(testX4)[1]))

# testX <- as.matrix(testX)

toc()

cat(">>>> Creating model and calculating probabilities... \n")
tic("Done")

model1 <- densityMclust(trainX1)
model2 <- densityMclust(trainX2)
model3 <- densityMclust(trainX3)
model4 <- densityMclust(trainX4)

PxC1 <- dens(modelName=model1$modelName, 
             data = testX, 
             parameters = model1$parameters)

PxC2 <- dens(modelName=model2$modelName, 
             data = testX, 
             parameters = model2$parameters)

PxC3 <- dens(modelName=model3$modelName, 
             data = testX, 
             parameters = model3$parameters)

PxC4 <- dens(modelName=model4$modelName, 
             data = testX, 
             parameters = model4$parameters)

PC1 = length(trainX1) / (length(trainX1) + length(trainX2) + length(trainX3) + length(trainX4))
PC2 = length(trainX2) / (length(trainX1) + length(trainX2) + length(trainX3) + length(trainX4))
PC3 = length(trainX3) / (length(trainX1) + length(trainX2) + length(trainX3) + length(trainX4))
PC4 = length(trainX4) / (length(trainX1) + length(trainX2) + length(trainX3) + length(trainX4))

toc()

cat(">>>> Classificating... ")
tic("Done")

result <- c()
PxC <- matrix(,nrow = nrow(testX), ncol = 4)
for (j in 1:dim(testX)[1]) {
    
    PxC[j,1] <- PxC1[j] * PC1
    PxC[j,2] <- PxC2[j] * PC2
    PxC[j,3] <- PxC3[j] * PC3
    PxC[j,4] <- PxC4[j] * PC4
}

trainResult <- max.col(PxC)
resultGMM <- matrix(checkAcc(testY, trainResult), ncol=2)
colnames(resultGMM) <- c("Success","Accuracy(%)")

results[["Result GMM"]] <- resultGMM

toc()
toc()
toc()