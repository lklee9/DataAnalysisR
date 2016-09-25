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

ConstructFrequencyMatrix <- function(data.all, attribute.values, class.values) {
  print("Constructing Frequency Matrix")
  br <- seq(min(data.all[, 1]), max(data.all[, 1]), (max(data.all[, 1]) - min(data.all[, 1]))/5)
  matrix.list <- Map(function(x) 
    hist(data.all[data.all[,2] == x, 1], breaks = br, include.lowest = T, plot = F)$counts, class.values)
  #for (idx_row in seq(1, nrow(data.all))) {
  #  i <- match(data.all[idx_row, 1], attribute.values)
  #  j <- match(data.all[idx_row, 2], class.values)
  #  matrix.frequency[i, j] <- matrix.frequency[i, j] + 1
  #}
  matrix.frequency <- data.frame(matrix.list)
  return(matrix.frequency)
}

SinglePosteriorCompare <- function(data.before, data.after, column.index) {
  data.frame.before <- data.frame(data.before[, column.index], data.before[, ncol(data.before)])
  data.frame.after <- data.frame(data.after[, column.index], data.after[, ncol(data.after)])
  
  attribute.values <- sort(unique(c(as.character(data.frame.before[, 1]), as.character(data.frame.after[, 1]))))
  class.values <- sort(unique(c(as.character(data.frame.before[, 2]), as.character(data.frame.after[, 2]))))
  
  matrix.before <- ConstructFrequencyMatrix(data.frame.before, attribute.values, class.values)
  matrix.after <- ConstructFrequencyMatrix(data.frame.after, attribute.values, class.values)
  
  matrix.difference <- abs(matrix.after - matrix.before)
  return(matrix.difference)
}

## ---- end-of-compare