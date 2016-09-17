library(foreign)

## ---- arff

ReadArffData <- function(file1, file2 = NA) {
  if (is.na(file2)) {
    return(SplitData(file1))
  }
  else {
    x <- read.arff(file1)
    y <- read.arff(file2)
    return(list(x, y))
  }
}

SplitData <- function(file_path) {
  data.original <- read.arff(file_path)
  data.1 <- data.original[1:floor(nrow(data.original)/2), ]
  data.2 <- data.original[(floor(nrow(data.original)/2) + 1):nrow(data.original), ]
  return(list(data.1, data.2))
}

## ---- end-of-arff