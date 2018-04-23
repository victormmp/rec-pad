rm(list=ls())

library("rgl")
library("mlbench")
library("simone")
source("mds.R")

normalize <- function(array) {return((array-min(array))/(max(array)-min(array)))}

data(BreastCancer)
data(cancer)
usa <- (USArrests)

usa$Murder <- normalize(usa$Murder)
usa$Assault <- normalize(usa$Assault)
usa$UrbanPop <- normalize(usa$UrbanPop)
usa$Rape <- normalize(usa$Rape)

bre <- apply(data.matrix(BreastCancer),2,normalize)

yusa <- MDS(usa,0.001)
ybreast <- MDS(usa,0.001)

plot(yusa[,1],yusa[,2])

