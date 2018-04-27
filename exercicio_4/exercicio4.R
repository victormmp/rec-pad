rm(list=ls())

library("rgl")
library("mlbench")
# library("simone")
source("mds.R")
aux <- gzfile("simone.rds")
simone <- readRDS(aux)

normalize <- function(array) {return((array-min(array))/(max(array)-min(array)))}

data(BreastCancer)
# data(cancer)
usa <- (USArrests)
bre <- (BreastCancer)

usa$Murder <- normalize(usa$Murder)
usa$Assault <- normalize(usa$Assault)
usa$UrbanPop <- normalize(usa$UrbanPop)
usa$Rape <- normalize(usa$Rape)
# bre <- apply(data.matrix(BreastCancer),2,normalize)
simone_aux <- simone$expr
simone <- apply(data.matrix(simone_aux),2,normalize)


## Parte 1

yusa_mds2 <- MDS(usa,0.001)
ycancer_mds2 <- MDS(simone,0.000001)

yusa_mds3 <- MDS(usa,0.001,dimensions = 3)
ycancer_mds3 <- MDS(simone,0.000001,dimensions = 3)

plot(yusa_mds2[,1],yusa_mds2[,2])
plot(ycancer_mds2[,1],ycancer_mds2[,2])



