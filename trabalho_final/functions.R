##################################
# plot - function(X,Y) {
#     xlim <- c(10,10)
#     plot(X,Y,xlim)
# }

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