print("=== Starting routine ===")
rm(list=ls())

#install.packages("http://cran.r-project.org/src/contrib/Archive/jpeg/jpeg_0.1-6.tar.gz", repo=NULL, type="source")

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library('jpeg')
library('plot3D')
library('rgl')
library('tictoc')

tic("Total Time")


# ======= Functions =======

rotate <- function(x) t(apply(x,2,rev))

contaCor <- function(x) {
  
  x <- as.integer(x)
  
  co <- array(0,10)
  
  for (i in x) {
    if (i < 0.1) co[1] <- co[1] + 1
    else if (i < 0.2) co[2] <- co[2] + 1
    else if (i < 0.3) co[3] <- co[3] + 1
    else if (i < 0.4) co[4] <- co[4] + 1
    else if (i < 0.5) co[5] <- co[5] + 1
    else if (i < 0.6) co[6] <- co[6] + 1
    else if (i < 0.7) co[7] <- co[7] + 1
    else if (i < 0.8) co[8] <- co[8] + 1
    else if (i < 0.9) co[9] <- co[9] + 1
    else if (i < 1) co[10] <- co[10] + 1
  }
  return(co)
}


features <- function(x){
  
  rows <- dim(x)[2]
  cols <- dim(x)[1]
  
  d <- 1*as.vector(x)
  
  m <- mean(x)
  t <- sum(x)
  s <- sd(x)
  
  co <- contaCor(x)
  
  co_m <- mean(co)
  co_t <- sum(co)
  co_s <- sd(co)
  
  zero_b <- 0
  while(x[zero_b+1] < 0.3) {
    zero_b <- zero_b + 1
  }
  
  zero_a <- 1
  while (x[length(x)-zero_a] < 0.1) {
    zero_a <- zero_a + 1
  }
  
  ter_r <- matrix(array(0,30), nrow=3, ncol=10)
  
  ter_r[1,] <- contaCor(x[,1:as.integer(rows/3)])
  ter_r[2,] <- contaCor(x[,as.integer(rows/3 + 1):as.integer(2*rows/3)])
  ter_r[2,] <- contaCor(x[,as.integer(2*rows/3 + 1):rows])
  
  ter_c <- matrix(array(0,3), nrow=3, ncol=10)
  
  ter_c[1,] <- contaCor(x[1:as.integer(cols/3),])
  ter_c[2,] <- contaCor(x[as.integer(cols/3 + 1):as.integer(2*cols/3),])
  ter_c[2,] <- contaCor(x[as.integer(2*cols/3 + 1):cols,])
  
  ter <- c()
  
  for (i in seq(3)) {
    ter <- c(ter,as.array(ter_c[i,]),as.array(ter_r[i,]))
  }
  
  
  return (c(m,t,s,co,co_m,co_t,co_s,zero_a,zero_b,ter))
}

features2 <- function(x) {
  d <- as.vector(x)
  size <- length(x)
  w <- seq(1,size,1)
  
  return (sum(w*x))
  
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

sim2 <- function(a,b) {
  
}


contorna <- function(pos,s,M) {
  y <- pos[1,1]
  x <- pos[1,2]
  
  rows <- s[1]
  cols <- s[2]
  
  seqx<-seq(x,x+cols-1,1)
  seqy<-seq(y,y+rows-1,1)
  M2<-M
  
  for(x in seqx){
    M2[y,x] = 1
    M2[y+rows-1,x] = 1
  }
  
  x = pos[1,2]
  
  for(y in seqy){
    M2[y,x] = 1
    M2[y,x+cols-1] = 1
  }
  
  par(new=TRUE)
  image(rotate(M2))
}

# ======== Routine ========

tic("Loading Images")

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

toc()
print("Calculating correlation for K")
tic("Time: ")

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

count <- 0

for (x in seqCol) {
  for(y in seqRow) {
    m <- Letter[y:(y+krow-1),x:(x+kcol-1)]
    fm <- features(m)
    fK <- features(K)
    Mcorr[y,x] <- sim(fK,fm)
    #Mcorr[y,x] <- sum(as.integer(xor(K,m)))
    #print(x)
    
    # print(c("Calculando para matrix: ", count))
    # count <- count + 1 
    
  }
}

toc()
print("Calculating correlation for S")
tic("Time: ")

# Para S

numRow <- lrow - srow
numCol <- lcol - scol

seqRow <- seq(1,numRow,1)
seqCol <- seq(1,numCol,1)

#print(lcol[1])

Mcorr_s <- matrix(nrow = numRow, ncol = numCol)

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

toc()
print("Finding letters")
tic("Time: ")

MM <- rotate(Mcorr)
seqxMM <- seq(1,nrow(MM),1)
seqyMM <- seq(1,ncol(MM),1)

# persp3D(seqxMM,seqyMM,MM)
# contour(seqxMM,seqyMM,MM)

pos <- which(Mcorr == min(Mcorr), arr.ind = TRUE)
pos_s <- which(Mcorr_s == min(Mcorr_s), arr.ind = TRUE)

y = pos[1,1]
x = pos[1,2]

y_s = pos_s[1,1]
x_s = pos_s[1,2]

toc()

#plot3D(Mcorr)

m <- Letter[y:(y+krow-1), x:(x+kcol-1)]
m_s <- Letter[y_s:(y_s+srow-1), x_s:(x_s+scol-1)]

#image(rotate(m_s))
# image(rotate(Letter))


contorna(pos,kdim,Letter)

contorna(pos_s,sdim,Letter)

toc()

print("=== Ended routine ===")
