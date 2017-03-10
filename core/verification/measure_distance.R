require(ggplot2)

ConstructFrequencyMatrix <- function(data.raw, attribute.index, class.values, bins) {
  data.all <- data.frame(data.raw[, attribute.index], data.raw[, ncol(data.raw)])
  if (length(bins) == 1) {
    bins <- EqualFreqBreaks(data.raw, attribute.index, bins)
  }
  freq.list <- lapply(class.values, 
                      function(x) as.numeric(hist(data.all[data.all[,2] == x, 1], breaks = bins, include.lowest = T, plot = F)$counts))
  
  matrix.frequency <- data.frame(freq.list)
  names(matrix.frequency) <- class.values
  matrix.frequency <- t(matrix.frequency)
  return(matrix.frequency)
}

EqualFreqBreaks <- function(data.all, column.index, bins.count) {
    return(ggplot2:::breaks(data.all[, column.index], "n", n = bins.count))
}

SingleLikelihoodDistance <- function(data.before, data.after, column.index) {
  print(paste0("Likelihood for ", column.index))
  min_val = min(min(data.before[, column.index]), min(data.after[, column.index]))
  max_val = max(max(data.before[, column.index]), max(data.after[, column.index]))
  br_gap = (max_val - min_val) / 50
  br <- seq(min_val, max_val, br_gap)
  
  class.values <- sort(unique(c(as.character(data.before[, ncol(data.before)]), 
                                as.character(data.after[, ncol(data.after)]))))
  
  matrix.before <- ConstructFrequencyMatrix(data.before, column.index, class.values, br)
  matrix.after <- ConstructFrequencyMatrix(data.after, column.index, class.values, br)
  
  class.freq.before <- table(data.before[, ncol(data.before)])
  class.freq.after <- table(data.after[, ncol(data.after)])
  
  matrix.before <- sapply(seq(1:ncol(matrix.before)),
                          function(x) matrix.before[, x] / class.freq.before[x])
  matrix.after <- sapply(seq(1:ncol(matrix.after)),
                          function(x) matrix.after[, x] / class.freq.after[x])
  matrix.difference <- abs(matrix.after - matrix.before)
  
  class.freq.before <- class.freq.before / nrow(data.before)
  class.freq.after <- class.freq.after / nrow(data.after)
  
  ret_dist <- sapply(seq(1:ncol(matrix.difference)),
                      function(x) sum(matrix.difference[, x]) *
                        (class.freq.before[x] + class.freq.after[x]) / 2)
  return(sum(ret_dist) / 2)
}

SinglePosteriorDistance <- function(data.before, data.after, column.index) {
  print(paste0("Posterior for ", column.index))
  min_val = min(min(data.before[, column.index]), min(data.after[, column.index]))
  max_val = max(max(data.before[, column.index]), max(data.after[, column.index]))
  br_gap = (max_val - min_val) / 50
  br <- seq(min_val, max_val, br_gap)
  
  class.values <- sort(unique(c(as.character(data.before[, ncol(data.before)]), 
                                as.character(data.after[, ncol(data.after)]))))
  
  matrix.before <- t(ConstructFrequencyMatrix(data.before, column.index, class.values, br))
  matrix.after <- t(ConstructFrequencyMatrix(data.after, column.index, class.values, br))
  
  att.freq.before <- sapply(seq(1:ncol(matrix.before)), 
                                function(x) sum(matrix.before[, x]))
  att.freq.after <- sapply(seq(1:ncol(matrix.after)), 
                                function(x) sum(matrix.after[, x]))
  
  matrix.difference <- sapply(seq(1:ncol(matrix.before)),
                              function(x) abs((matrix.before[,x] / att.freq.before[x]) - 
                                                  (matrix.after[,x] / att.freq.after[x])))
  matrix.difference[is.nan(matrix.difference)] <- 0
  
  att.freq.before <- att.freq.before / nrow(data.before)
  att.freq.after <- att.freq.after / nrow(data.after)
  
  ret_dist <- sapply(seq(1:ncol(matrix.difference)),
                      function(x) sum(matrix.difference[, x]) *
                        (att.freq.before[x] + att.freq.after[x]) / 2)
  return(sum(ret_dist) / 2)
}

allPosteriorDist <- function(data.before, data.after) {
  attributes.all <- unique(c(names(data.before)[1:(ncol(data.before)-1)],
                             names(data.after)[1:(ncol(data.after)-1)]))
  distance.all <- sapply(seq(1:length(attributes.all)),
                         function(x) SinglePosteriorDistance(data.before, data.after, x))
  print(distance.all)
  names(distance.all) <- attributes.all
  distance.all <- sort(distance.all)
  return(distance.all)
}
