require(ggplot2)
require(ggrepel)
library(reshape2)
library(gridExtra)
library(grid)
source("visualisation.R")

## ---- cond_covar

GetDataByClass <- function(data.all, class.value) {
  return(data.all[data.all[, length(data.all)] == class.value, ])
}

GetDataByAttribute <- function(data.all, attribute.name) {
  attribute.all <- sapply(strsplit(as.character(data.all[, length(data.all) - 1]), "_"), function(x) x[[1]][1])
  attribute.data <- data.all[
    attribute.all == attribute.name, ]
  attribute.data <- attribute.data[!is.infinite(attribute.data[, 1]), ]
  return(attribute.data)
}

GetAllAttributes <- function(data.all) {
  attribute.list <- as.character(data.all[, length(data.all) - 1])
  attributes.all <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  attributes.factor <- as.factor(attributes.all)
  return(levels(attributes.factor))
}

JointVisualisation <- function(data.all) {
  distance <- data.all$Distance
  class_value <- data.all$class_value
  attribute.list <- as.character(data.all[, length(data.all) - 1])
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  
  plot.data <- data.frame(distance, class_value, attributes)
  names(plot.data) <- c("distance", "class_value", "attributes")
  
  # To change the color of the gradation :
  #plot <- ggplot(plot.data, aes(class_value, attributes, z= distance)) + 
  #  geom_tile(aes(fill = distance)) + 
  #  theme_bw() + 
  #  scale_fill_gradient(low="green", high="red") +
  #  ggtitle("Conditioned Covariate Drift")
    
  plot <- plot_ly(plot.data,
                  x = ~class_value, 
                  y = ~attributes, 
                  z = ~distance, 
                  colors = colorRamp(c("green", "red")),
                  width = 1000,
                  height = 750,
                  type = "heatmap")
  
  return(plot)
}

PairwiseAttributes <- function(data.att1, data.att2) {
  class.all <- unique(data.att1[, length(data.att1)])
  plot.list <- list()
  z_min <- min(data.att1[,1])
  z_max <- max(data.att1[,1])
  for (i in 1:length(class.all)) {
    data.mono <- GetDataByClass(data.att1, class.all[i])
    data.dual <- GetDataByClass(data.att2, class.all[i])
    plot <- RawDataToHeatMap(data.mono, data.dual, class.all[i], z_min, z_max)
    if (length(plot) > 1 || !is.na(plot)) {
      plot.list[[length(plot.list) + 1]] <- plot
    }
    else {
      print("Invalid plot at:")
      print(names(data1)[i])
      print(plot)
    }
  }
  print("Plots obtained")
  
  n <- length(plot.list)
  nCol <- ceiling(sqrt(n))
  g <- layout(subplot(plot.list, nrows = ceiling(n / nCol),
                      shareX = TRUE, shareY = TRUE),
              showlegend = FALSE, title = paste(class.all, collapse = "_"))
  return(g)
}

## ---- end-of-cond_covar