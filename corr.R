corr <- function(directory, threshold = 0){
  completeData = complete(directory)
  id <- completeData["id"][completeData["nobs"] > threshold]
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  filelist <- list.files(dir)
  result <- vector()
  
  if(length(id) == 0){
    return(as.numeric(result))
  }
  
  for(i in 1:length(id)){
    con <- file(paste(dir, filelist[id[i]], sep = ""), "r")
    initial <- read.table(con, nrows = 100)
    classes <- sapply(initial, class)
    data <- read.table(con, sep = ",", colClasses = classes)
    close(con)
    names(data) <- c("Date", "sulfate", "nitrate", "id")
    sulfateNa <- data["sulfate"]
    nitrateNa <- data["nitrate"]
    s <- !is.na(sulfateNa)
    n <- !is.na(nitrateNa)
    sulfate <- as.numeric(sulfateNa[s & n])
    nitrate <- as.numeric(nitrateNa[s & n])
    result <- c(result, cor(sulfate, nitrate))
  }
  result
}
