require(ggplot2)
require(ggrepel)
library(reshape2)
library(gridExtra)
library(grid)
library(arules)

## ---- compare

Histogram <- function(data1, data2, n.bin = NA) {
  plot.list <- list()
  for (i in 1:length(data1)) {
    plot <- SingleHistogram(data1, data2, i, n.bin)
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
  print(length(plot.list))
  if (length(plot.list) > 1) {
    grid_arrange_shared_legend(plot.list)
  }
  else {
      return(plot.list[[1]])
  }
}

SingleHistogram <- function(data1, data2, column.index, n.bin = NA) {
    data.all.1 <- data.frame(data1[, column.index])
    data.all.2 <- data.frame(data2[, column.index])
    if (!is.na(n.bin) & !is.factor(data.all.1[,1])) {
      data.all.vec <- discretize(c(data.all.1[, 1], data.all.2[,1]), method = "frequency", categories = n.bin, ordered = TRUE)
      data.all.1[, 1] <- data.all.vec[seq(1,nrow(data.all.1))]
      data.all.2[, 1] <- data.all.vec[seq((nrow(data.all.1) + 1), (nrow(data.all.1) + nrow(data.all.2)))]
    }
    data.all.1$type <- "before"
    data.all.2$type <- "after"
    column.name <- names(data1)[column.index]
    names(data.all.1)[1] <- "value"
    names(data.all.2)[1] <- "value"
    data.all <- rbind(data.all.1, data.all.2)
    plot <- NA
    if (is.factor(data.all[, 1])) {
      plot <- ggplot(data.all, aes(x=value, fill = type)) + geom_bar(aes(y=..count../sum(..count..)), width = 1, alpha = 0.5, position = 'dodge')
      plot <- plot + labs(xlab(column.name))
    }
    else {
      if ((max(data.all[,1]) - min(data.all[,1])) > 0) {
        plot <- ggplot(data.all, aes(value, fill = type)) + 
          geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", binwidth = ((max(data.all[,1]) - min(data.all[,1]))/50))
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
## ---- end-of-compare