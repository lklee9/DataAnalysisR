## ---- distance_matrix

# Public ----

PlotMDS <- function(data.table) {
  attributes.all <- ExtractAttributes(resultTable)
  distance.matrix <- DistanceMatrix(data.table)
  MDS(distance.matrix, attributes.all)
}

# Private ----

DistanceMatrix <- function(data.table) {
  attributes <- ExtractAttributes(data.table)
  distanceMatrix <- matrix(0, nrow = length(attributes), ncol = length(attributes))
  distToAttPair <- getDistToAttPair(data.table)
  distanceMatrix <- AddToDistance(distanceMatrix, distToAttPair)
  return(distanceMatrix)
}

ExtractAttributes <- function(data.table) {
  data.attributes <- as.character(data.table$attributes)
  attributes.separated <- Reduce(function(x, y) Map(c, x, strsplit(y, "_")), data.attributes, init="")[[1]]
  attributes.all <- Filter(function(x) x != "", attributes.separated)
  attributes <- unique(attributes.all)
  attributes <- Filter(function(x) x != "id", attributes)
  return(attributes)
}

getDistToAttPair <- function(data.table) {
  attributes <- ExtractAttributes(data.table)
  data.attributes <- as.character(data.table$attributes)
  distToAttPair <- Map(
    function(x) Map(
      function(y) Map(
        function(z) c(data.table$drift[x], 
                      match(y, attributes), match(z, attributes)), 
        strsplit(data.attributes[x], "_")[[1]]
        ),
      strsplit(data.attributes[x], "_")[[1]]
      ),
    seq(1:nrow(data.table))
    )
  distToAttPair <- data.frame(distToAttPair)
  distToAttPair <- Filter(function(x) 
    !is.na(x[1]) && !is.na(x[2]) && !is.na(x[3]) && x[2] != x[3], distToAttPair)
  names(distToAttPair) <- seq(1:length(distToAttPair))
  return(distToAttPair)
}

AddToDistance <- function(distanceMatrix, distToAttPair) {
  d <- distToAttPair[1, 1]
  i <- distToAttPair[2, 1]
  j <- distToAttPair[3, 1]
  distanceMatrix[i, j] <- distanceMatrix[i, j] + d
  if (length(distToAttPair) <= 1) {
    return(distanceMatrix)
  }
  else {
    if (length(distToAttPair) %% 2 == 1) {
      distanceMatrix <- AddToDistance(distanceMatrix, distToAttPair[2:((length(distToAttPair) + 1)/2)])
      distanceMatrix <- AddToDistance(distanceMatrix, distToAttPair[((length(distToAttPair) + 1)/2):length(distToAttPair)])
      return(distanceMatrix)
    }
    else {
      return(AddToDistance(distanceMatrix, distToAttPair[2:length(distToAttPair)]))
    }
  }
}

## ---- end-of-distance_matrix