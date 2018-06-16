##################################
# plot - function(X,Y) {
#     xlim <- c(10,10)
#     plot(X,Y,xlim)
# }

##################################
    
checkAcc <- function(result, correct) {
    
    if (length(result) != length(correct)){
        cat("[checkAcc Error] Sizes of result and correct are diferent! Please check pparameters.")
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
    dim_classe <- 10
    numClasses <- 2
    numAmostras <- 10
    
    seqN <- sample(numAmostras)
    porcAmostTrain <- 0.5
    
    N <- seqN[1:(porcAmostTrain*numAmostras)]
    nSamplesTrain <- length(N)
    n <- seqN[(porcAmostTrain*numAmostras+1):numAmostras]
    nSamplesTest <- length(n)
    
    xtreino <- c()
    xtreinoPCA <- c()
    xtreinoMDS <- c()
    ytreino <- c()
    xteste <- c()
    xtestePCA <- c()
    xtesteMDS <- c()
    yteste <- c()
    
    for(r in seq(1,numClasses,numAmostras)) {
        for(i in N) {
            xtreino <- rbind(xtreino, (faces[r+i-1,]))
            xtreinoPCA <- rbind(xtreinoPCA, (facesPCA[r+i-1,]))
            xtreinoMDS <- rbind(xtreinoMDS, (facesMDS[r+i-1,]))
            ytreino <- c(ytreino,(y[r+i-1]))
        }
        
        for(i in n) {
            xteste <- rbind(xteste, (faces[r+i-1,]))
            xtestePCA <- rbind(xtestePCA, (facesPCA[r+i-1,]))
            xtesteMDS <- rbind(xtesteMDS, (facesMDS[r+i-1,]))
            yteste <- c(yteste,(y[r+i-1]))
        }
    }
}