##################################
plotBar <- function(results) {
    
    # png(filename = "results.png")
    plots <- c(results$KNNPCA[1,2],
               results$KNNMDS[1,2],
               results$Bayes[1,2],
               results$Bayes[2,2],
               results$GMMPCA[1,2],
               results$GMMMDS[1,2])
    plots_names <- c("KNN e PCA", "KNN e MDS", "Bayes e PCA", "Bayes e MDS", "GMM e PCA", "GMM e MDS")
    # barplot(height=(plots),space = 0.3,width = 1,ylim = c(0,100),beside = TRUE, names.arg = plots_names)
    
    
    # Basic Barplot
    my_bar=barplot(height=(plots) , 
                   border=F , 
                   names.arg=plots_names ,
                   cex.names = 0.7 ,
                   las=2 ,
                   ylim=c(0,150) ,
                   col=c(rgb(0.3,0.1,0.4,0.6) , 
                         rgb(0.3,0.5,0.4,0.6) , 
                         rgb(0.3,0.9,0.4,0.6) ,  
                         rgb(0.1,0.4,0.7,0.6) ,
                         rgb(0.4,0.5,0.4,0.6) ,
                         rgb(0.1,0.2,0.9,0.6)),
                   main="Exatidão dos Métodos" )
    # abline(v=c(4.9 , 9.7) , col="grey")
    
    # Add the text 
    text(my_bar, plots + 10 , paste(round(plots, digits=2),"%",sep="") ,cex=0.7) 
    
    #Legende
    # legend("topleft", 
    #        legend = plots_names ,
    #        col=c(rgb(0.3,0.1,0.4,0.6) , 
    #              rgb(0.3,0.5,0.4,0.6) , 
    #              rgb(0.3,0.9,0.4,0.6) ,  
    #              rgb(0.1,0.4,0.7,0.6) ,
    #              rgb(0.4,0.5,0.4,0.6)) ,
    #        bty = "n", 
    #        pch=20 , 
    #        pt.cex = 2, 
    #        cex = 0.8, 
    #        horiz = FALSE, 
    #        inset = c(0.05, 0.05))
    
    # dev.copy(png,filename="results.png");
    # dev.off()
}

##################################

getGlobalVariables <- function() {
    cat(">> Var 1: ", TRAIN_SET_SIZE)
}

checkAcc <- function(result, correct) {
    
    if (length(result) != length(correct)){
        cat("[checkAcc Error] Sizes of result and correct are diferent! Please check parameters.")
        return(NULL)
    }
    
    count <- 0
    for (i in seq(length(result))) {
        if (result[i] == correct[i]) count <- count + 1
    }
    
    percCorrect <- count / length(result) * 100
    
    return(c(count, percCorrect))
}

#################################

splitClasses <- function() {
    
    numAmostras <- nrow(x)
    seqN <- sample(numAmostras)
    
    N <- seqN[1:(TRAIN_SET_SIZE*numAmostras)]
    nSamplesTrain <- length(N)
    n <- seqN[(TRAIN_SET_SIZE*numAmostras+1):numAmostras]
    nSamplesTest <- length(n)
    
    xtreino <<- c()
    xtreinoPCA <<- c()
    xtreinoMDS <<- c()
    ytreino <<- c()
    xteste <<- c()
    xtestePCA <<- c()
    xtesteMDS <<- c()
    yteste <<- c()
    
    for(i in N) {
        xtreino <<- rbind(xtreino, (x[i,]))
        xtreinoPCA <<- rbind(xtreinoPCA, (xPCA[i,]))
        xtreinoMDS <<- rbind(xtreinoMDS, (xMDS[i,]))
        ytreino <<- rbind(ytreino,(y[i,]))
    }
    
    for(i in n) {
        xteste <<- rbind(xteste, (x[i,]))
        xtestePCA <<- rbind(xtestePCA, (xPCA[i,]))
        xtesteMDS <<- rbind(xtesteMDS, (xMDS[i,]))
        yteste <<- rbind(yteste,(y[i,]))
    }
    
}

#######################################################

GMM <- function(X, y, method) {
    cat(">>>> Generating indexes... ")
    tic("Done")
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
    
    trainX1 <- X[indexC1[index1[1:(TRAIN_SET_SIZE*length(index1))]],]
    testX1 <- X[indexC1[index1[(TRAIN_SET_SIZE*length(index1) + 1):length(index1)]],]
    
    trainX2 <- X[indexC2[index2[1:(TRAIN_SET_SIZE*length(index2))]],]
    testX2 <- X[indexC2[index2[(TRAIN_SET_SIZE*length(index2) + 1):length(index2)]],]
    
    trainX3 <- X[indexC3[index3[1:(TRAIN_SET_SIZE*length(index3))]],]
    testX3 <- X[indexC3[index3[(TRAIN_SET_SIZE*length(index3) + 1):length(index3)]],]
    
    trainX4 <- X[indexC4[index4[1:(TRAIN_SET_SIZE*length(index4))]],]
    testX4 <- X[indexC4[index4[(TRAIN_SET_SIZE*length(index4) + 1):length(index4)]],]
    
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
    rownames(resultGMM) <- method
    
    toc()
    
    return(resultGMM)
}