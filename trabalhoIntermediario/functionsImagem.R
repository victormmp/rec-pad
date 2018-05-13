#' MostraImagem: Funcao responsavel por renderizar um vetor de puxels
#'
#' @param x: vetor de pixels relativo a imagem
MostraImagem <- function( x )
{
    
    img <- matrix( x, nrow=64 )
    cor <- rev( gray(50:1/50) )
    image( rotate( img ), col=cor )
}
# MostraImagem( faces[1,] )

pdfnvar <- function(x,m,K,n) {
    
    y <- ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
    return(y)
    
}

scalar1 <- function(x) {
    y <- x
    for (i in seq(nrow(x))){
        y[i,] <- x[i,] / sqrt(sum(x[i,]^2))
    }
    return(y)
    
}

bayes <- function(xtreino, ytreino, nSamples, xteste, yteste) {
    
    Mcov <- list()
    nClass <- nrow(xtreino) / nSamples
    classe <- 1
    meansList <- list()
    nFeatures <- ncol(xtreino)
    
    for (i in seq(1,nrow(xtreino), nSamples)) {
        Mcov[[classe]] <- cov(xtreino[i:(i+nSamples),])
        meansList[[classe]] <- colMeans(xtreino[i:nSamples-1,])
        classe <- classe + 1
    }
    
    predResult <- matrix(nrow = nrow(xteste))
    currentPdf <- 0
    
    for (i in seq(nrow(xteste))) {
        for (j in seq(nClass)) {
            pdf <- pdfnvar(xteste[i,],meansList[[j]],Mcov[[j]],nFeatures)
            if (pdf >= currentPdf) {
                predResult[i] <- j
                currentPdf <- pdf
            }
        }
    }
    
    # return(predResult)
    return(Mcov)
}