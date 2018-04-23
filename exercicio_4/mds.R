MDS<-function(X,eta,dimensions = 2,itermax = 500, tol = 0.0001){
  X<-data.matrix(X)
  M<-as.matrix(dist(X))
  N<-nrow(M)
  
  decimals <- seq(from=-2, to=2, by=.1)
  
  x_old<-c()
  for (i in 1:N){
    x_old<-rbind(x_old,sample(decimals,dimensions))
  }
  D<-as.matrix(dist(x_old))
  
  L0<-0
  for (i in 2:N) {
    for (j in 1:(i-1)) {
      L0<-L0+1/2*(M[i,j]^2-D[i,j]^2)^2
    }
  }
  
  stop_cond<-FALSE
  erro<-2*L0
  iter<-0
  
  while (stop_cond==FALSE) {
    
    DELTA<-matrix(0,N,dimensions)
    for (k in 1:N) {
      for (p in 1:dimensions) {
        delta<-0
        for (i in 1:N) {
          delta<-delta + (M[k,i]^2-D[k,i]^2)*(x_old[k,p]-x_old[i,p])
        }
        DELTA[k,p]<-2*eta*delta
      }
    }
    x_new <- x_old + DELTA
    x_old <- x_new
    
    D<-as.matrix(dist(x_old))
    L1<-0
    for (i in 2:N) {
      for (j in 1:(i-1)) {
        L1<-L1+1/2*(M[i,j]^2-D[i,j]^2)^2
      }
    }
    
    MAX_DELTA<-max(abs(DELTA))
    erro[iter+2]<-L1
    stop1<-(MAX_DELTA<tol)
    stop2<-(iter>itermax)
    stop_cond<-(stop1 | stop2)
    iter<-iter+1
  }
  return(x_new)
}
