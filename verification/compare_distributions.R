require(ggplot2)
require(ggrepel)
library(reshape2)
library(gridExtra)
library(grid)
library(arules)
source("visualisation.R")

## ---- compare

Histogram <- function(data1, data2) {
  plot.list <- list()
  for (i in 1:length(data1)) {
    plot <- SingleHistogram(data1, data2, i)
    if (length(plot) > 1 || !is.na(plot)) {
      plot.list[[length(plot.list) + 1]] <- plot
    }
    else {
      print("Invalid plot at:")
      print(names(data1)[i])
      print(plot)
    }
  }
  print("Plots Obtained")
  grid_arrange_shared_legend(plot.list)
}

SingleHistogram <- function(data1, data2, column.index) {
    data.all.1 <- data.frame(data1[, column.index])
    data.all.2 <- data.frame(data2[, column.index])
    data.all.1$type <- "before"
    data.all.2$type <- "after"
    column.name <- names(data1)[column.index]
    names(data.all.1)[1] <- "value"
    names(data.all.2)[1] <- "value"
    data.all <- rbind(data.all.1, data.all.2)
    plot <- NA
    if (is.factor(data.all[, 1])) {
      plot <- ggplot(data.all, aes(value, fill = type)) + geom_bar(width = 1, alpha = 0.5, position = 'dodge')
      plot <- plot + labs(xlab(column.name))
    }
    else {
      if ((max(data.all[,1]) - min(data.all[,1])) > 0) {
        plot <- ggplot(data.all, aes(value, fill = type)) + 
          geom_histogram(alpha = 0.5, position = "identity", binwidth = ((max(data.all[,1]) - min(data.all[,1]))/50))
          #geom_histogram(alpha = 0.5, position = "identity", binwidth = 10)
        plot <- plot + labs(xlab(column.name))
      }
      else {
        plot <- ggplot(data.all, aes(value, fill = type)) + 
          geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.1)
        plot <- plot + labs(xlab(column.name))
      }
    }
    return(plot)
}

ConstructFrequencyMatrix <- function(data.raw, attribute.index, class.values, bins) {
  data.all <- data.frame(data.raw[, attribute.index], data.raw[, ncol(data.raw)])
  matrix.list <- Map(function(x) 
    hist(data.all[data.all[,2] == x, 1], breaks = bins, include.lowest = T, plot = F)$counts, class.values)
  matrix.frequency <- data.frame(matrix.list)
  return(matrix.frequency)
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

## ---- end-of-compare