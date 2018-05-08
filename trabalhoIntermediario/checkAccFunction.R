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