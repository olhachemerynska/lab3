complete <- function(directory, id = 1:332) {
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  filelist <- list.files(dir)
  ids <- vector()
  nobs <- vector()
  
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
    ids <- c(ids, id[i])
    nobs <- c(nobs, as.numeric(length(sulfateNa[s & n])))
  }
  data.frame("id" = ids, "nobs" = nobs)
  }
