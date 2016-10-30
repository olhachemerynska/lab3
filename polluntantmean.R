pollutantmean <- function(directory, pollutant, id = 1:332){
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  filelist <- list.files(dir)
  result <- 0
  
  for(i in 1:length(id)){
    con <- file(paste(dir, filelist[id[i]], sep = ""), "r")
    initial <- read.table(con, nrows = 100)
    classes <- sapply(initial, class)
    data <- read.table(con, sep = ",", colClasses = classes)
    close(con)
    names(data) <- c("Date", "sulfate", "nitrate", "id")
    pollutantdata <- data[pollutant]
    result <- result + mean(as.numeric(pollutantdata[!is.na(pollutantdata)]))
  }
  result
}
