rm(list=ls())

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library("tictoc")
library("class")
library("stats")
library("corpcor")
library("vbmp")
source("functions.R")
library("caret")

cat("\014")
tic("Total elapsed time")

cat(">> Reading Data... ")
tic("Done")

########## CONSTANTS ##########

NUMBER_OF_SAMPLES = 500

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

########## 

toc()



