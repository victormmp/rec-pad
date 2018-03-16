print("=== Starting routine ===")
rm(list=ls())

#install.packages("http://cran.r-project.org/src/contrib/Archive/jpeg/jpeg_0.1-6.tar.gz", repo=NULL, type="source")

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library('jpeg')
library('plot3D')
library('rgl')

rotate <- function(x) t(apply(x,2,rev))


features <- function(x){
  d <- 1*as.vector(x)
  return (d)
}


sim <- function(a,b){
  if (length(a) == length(b)){
    size <- length(a)[1]
    ed <- 0
    for (i in seq(size)){
      ed <- ed + (a[i]-b[i])**2
    }
    ed<- sqrt(ed)
    return (ed)
  }
}


kl <- readJPEG('./K.JPG')
K <- as.matrix(kl[,,3])
#K <- (K<0.75)

sl <- readJPEG('./S.JPG')
S <- as.matrix(sl[,,3])
#S <- (S<0.75)

Letterl <- readJPEG('./letters.JPG')
Letter <- as.matrix(Letterl[,,3])
#Letter <- (Letter<0.75)

#image(rotate(K))

kdim <- dim(K)
krow <- kdim[1]
kcol <- kdim[2]

sdim <- dim(S)
srow <- sdim[1]
scol <- sdim[2]

ldim <- dim(Letter)
lrow <- ldim[1]
lcol <- ldim[2]

numRow <- lrow - krow
numCol <- lcol - kcol

seqRow <- seq(1,numRow,1)
seqCol <- seq(1,numCol,1)

#print(lcol[1])

Mcorr <- matrix(nrow = numRow, ncol = numCol)

print("Loaded Images. Starting to create the correlate for K.")

for (x in seqCol) {
  for(y in seqRow) {
    m <- Letter[y:(y+krow-1),x:(x+kcol-1)]
    fm <- features(m)
    fK <- features(K)
    Mcorr[y,x] <- sim(fK,fm)
    #Mcorr[y,x] <- sum(as.integer(xor(K,m)))
    #print(x)
  }
}

# Para S

numRow <- lrow - srow
numCol <- lcol - scol

seqRow <- seq(1,numRow,1)
seqCol <- seq(1,numCol,1)

#print(lcol[1])

Mcorr_s <- matrix(nrow = numRow, ncol = numCol)

print("Correlate matrice for K created. Starting to create correlate for S.")

for (x in seqCol) {
  for(y in seqRow) {
    m_s <- Letter[y:(y+srow-1),x:(x+scol-1)]
    fm <- features(m_s)
    fS <- features(S)
    Mcorr_s[y,x] <- sim(fS,fm)
    #Mcorr[y,x] <- sum(as.integer(xor(K,m)))
    #print(x)
  }
}

print("Correlate for S created. Starting to identify the letters positions.")

MM <- rotate(Mcorr)
seqxMM <- seq(1,nrow(MM),1)
seqyMM <- seq(1,ncol(MM),1)

persp3D(seqxMM,seqyMM,MM)
#contour(seqxMM,seqyMM,MM)

pos <- which(Mcorr == min(Mcorr), arr.ind = TRUE)
pos_s <- which(Mcorr_s == min(Mcorr_s), arr.ind = TRUE)

y = pos[1,1]
x = pos[1,2]

y_s = pos_s[1,1]
x_s = pos_s[1,2]

#plot3D(Mcorr)

m <- Letter[y:(y+krow-1), x:(x+kcol-1)]
m_s <- Letter[y_s:(y_s+srow-1), x_s:(x_s+scol-1)]

image(rotate(m_s))
#image(rotate(Letter))

print("=== Ended routine ===")