readFaces <- function (dir) {
  #Leitura de dados ######################################################################
  xt <- c()
  
  xe <- c()
  
  
  for (i in 1:5) {
    for (j in 1:5) {
      path_xt <-
        paste(dir, paste(
          paste("/faces/f", i, sep = ""),
          paste(paste("teste", j, sep = ""), ".bmp", sep = ""),
          sep = ""
        ) , sep = "")
      
      path_xe <-
        paste(dir, paste(
          paste("/faces/F", i, sep = ""),
          paste(paste("TESTE", j, sep = ""), "R.BMP", sep = ""),
          sep = ""
        ), sep = "")
      
      
      img_train <- read.bmp(path_xt)
      
      img_test <- read.bmp(path_xe)
      
      
      xt <- rbind(xt, t(as.matrix(as.vector(img_train))))
      xe <- rbind(xe, t(as.matrix(as.vector(img_test))))
    }
  }
  
  return(rbind(xt,xe))
}