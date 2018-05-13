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
    
    y <- ((1/(((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(pseudoinverse(K))%*%(x-m))))
    return(y)
    
}

pdfNormalNVar = function(testVec, trainMean, covMat, n)
{
    aux_0 = 1 / (((2 * pi)^n) * det(covMat))
    aux_1 = -1/2 * t(testVec - trainMean) %*% solve(covMat) %*% (testVec - trainMean)
    return(aux_0 * exp(aux_1))
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
        Mcov[[classe]] <- cov(xtreino[i:(i+nSamples-1),])
        meansList[[classe]] <- colMeans(xtreino[i:(nSamples-1),])
        classe <- classe + 1
    }
    
    pdf <- matrix(nrow = nrow(xteste), ncol = nClass)
    
    for (i in seq(nrow(xteste))) {
        for (j in seq(nClass)) {
            pdf[i,j] <- pdfnvar(xteste[i,],meansList[[j]],Mcov[[j]],nFeatures)
        }
    }
    
    predResult <- matrix(nrow = nrow(pdf))
    predResult <- apply(pdf, 1,which.max)
    
    results <- list()
    checkAcc <- predResult - yteste
    win <- sum(checkAcc == 0)
    predWin <- win / length(yteste) * 100
    
    results[["predResult"]] <- predResult
    results[["predWin"]] <- predWin
    
    
    return(results)
}

bayesTest <- function(trainData, testData, trainClass) {
    nFeat <- dim(trainData)[2]
    nClass <- length(unique(trainClass))
    
    trainData_mean <- array(0, dim = c(nFeat, nClass))
    trainData_cov <- array(0, dim = c(nFeat, nFeat, nClass))
    
    for(i in seq(1, dim(trainData)[1], 5)) {
        trainData_mean[,trainClass[i]] = apply((trainData[i:(i + 5 - 1),]), 2, mean)
        trainData_cov[,,trainClass[i]] = cov(trainData[i:(i + 5 - 1),])
    }
    
    
    testData_ver <- array(0, dim = c(nClass, dim(testData)[1]))
    for(i in 1:nClass) {
        for(j in 1:dim(testData)[1]) {
            testData_ver[i,j] <- pdfNormalNVar(testData[j,], trainData_mean[,i], trainData_cov[,,i])
        }
    }
    
    return( factor(apply(testData_ver, 2, which.max), levels = 1:nClass) )
}
