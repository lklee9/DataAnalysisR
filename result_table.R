## ---- results
# Public ---------------------------------------------------

# A table that contains only distances for a set 
# of value combination for a set of attributes
ResultTable <- function(file.path, class.name=NA){
  file.path.vector <- strsplit(file.path, "_", fixed=TRUE)
  result.name <- head(file.path.vector, n=1)
  result.type <- tail(file.path.vector, n=1)
  result.n.attributes <- ifelse(result.type != "class", 
                                file.path.vector[2], 0)
  result.data <- ReadData(file.path, class.name)
  return(result.data)
}


# Private --------------------------------------------------

# Read csv file into data frame
ReadData <- function(file.path, class.name) {
  data.csv <- read.csv(file.path)
  column.last.name <- tail(names(data.csv), n=1)
  if (column.last.name == "class") {
    if (is.na(class.name)) 
      class.name <- data.csv$column.last.name[1]
    data.raw <- data.csv[data.csv$column.last.name == class.name]
  }
  else {
    data.raw <- data.csv
  }
  return(data.raw)
}

## ---- end-of-results