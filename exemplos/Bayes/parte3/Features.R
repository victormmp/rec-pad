# Functions

contaCor <- function(x) {
  x <- as.array(x)
  
  co <- array(0, 10)
  
  for (i in x) {
    if (i < 10)
      co[1] <- co[1] + 1
    else if (i < 20)
      co[2] <- co[2] + 1
    else if (i < 30)
      co[3] <- co[3] + 1
    else if (i < 40)
      co[4] <- co[4] + 1
    else if (i < 50)
      co[5] <- co[5] + 1
    else if (i < 60)
      co[6] <- co[6] + 1
    else if (i < 70)
      co[7] <- co[7] + 1
    else if (i < 80)
      co[8] <- co[8] + 1
    else if (i < 90)
      co[9] <- co[9] + 1
    else if (i < 100)
      co[10] <- co[10] + 1
  }
  return(co)
}

features1 <- function(x) {
  m <- mean(x)
  t <- sum(x)
  s <- sd(x)
  
  co <- contaCor(x)
  
  return(c(m, t, s, co))
}

features2 <- function(x) {
  d <- matrix(x, nrow = 56)
  v1 <- d[1:as.integer(dim(d)[1] / 2),]
  v2 <- d[as.integer(dim(d)[1] / 2 + 1):dim(d)[1],]
  
  v2 <- as.vector(v2)
  v1 <- as.vector(v1)
  
  f <- sim(v1, v2)
  m <- mean(x)
  t <- sum(x)
  s <- sd(x)
  
  return(c(f, m, t, s))
}

features3 <- function(x) {
  d <- matrix(x, nrow = 56)
  v1 <- d[1:as.integer(dim(d)[1] / 2),]
  v2 <- d[as.integer(dim(d)[1] / 2 + 1):dim(d)[1],]
  v3 <- d[, 1:as.integer(dim(d)[2] / 2)]
  v4 <- d[, as.integer(dim(d)[2] / 2 + 1):dim(d)[2]]
  
  v4 <- as.vector(v4)
  v3 <- as.vector(v3)
  v2 <- as.vector(v2)
  v1 <- as.vector(v1)
  
  f1 <- sim(v1, v2)
  f2 <- sim(v3, v4)
  f3 <- sim(v1, v3)
  f4 <- sim(v2, v4)
  f5 <- sim(v1, v4)
  f6 <- sim(v2, v3)
  
  return(c(f1, f2, f3, f4, f5, f6))
}

features4 <- function(x) {
  d <- matrix(x, nrow = 56)
  v1 <- d[1:as.integer(dim(d)[1] / 2),]
  v2 <- d[as.integer(dim(d)[1] / 2 + 1):dim(d)[1],]
  v3 <- d[, 1:as.integer(dim(d)[2] / 2)]
  v4 <- d[, as.integer(dim(d)[2] / 2 + 1):dim(d)[2]]
  
  v4 <- as.vector(v4)
  v3 <- as.vector(v3)
  v2 <- as.vector(v2)
  v1 <- as.vector(v1)
  
  f1 <- sim(v1, v2)
  f2 <- sim(v3, v4)
  f3 <- sim(v1, v3)
  f4 <- sim(v2, v4)
  f5 <- sim(v1, v4)
  f6 <- sim(v2, v3)
  
  m <- mean(x)
  t <- sum(x)
  s <- sd(x)
  
  mf <- mean(c(f1, f2, f3, f4, f5, f6))
  tf <- sum(c(f1, f2, f3, f4, f5, f6))
  sf <- sd(c(f1, f2, f3, f4, f5, f6))
  
  return(c(f1, f2, f3, f4, f5, f6, m, t, s, mf, tf, sf))
}

sim <- function(a, b) {
  if (length(a) == length(b)) {
    size <- length(a)[1]
    ed <- 0
    for (i in seq(size)) {
      ed <- ed + (a[i] - b[i]) ** 2
    }
    ed <- sqrt(ed)
    return (ed)
  }
}