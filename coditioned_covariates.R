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
  plot <- ggplot(plot.data, aes(class_value, attributes, z= distance)) + 
    geom_tile(aes(fill = distance)) + 
    theme_bw() + 
    scale_fill_gradient(low="green", high="red") +
    ggtitle("Conditioned Covariate Drift")
  
  return(plot)
}

PairwiseAttributes <- function(data.att1, data.att2) {
  class.all <- unique(data.att1[, length(data.att1)])
  plot.list <- list()
  for (i in 1:length(class.all)) {
    data.mono <- GetDataByClass(data.att1, class.all[i])
    data.dual <- GetDataByClass(data.att2, class.all[i])
    plot <- RawDataToHeatMap(data.mono, data.dual, class.all[i])
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
  grid_arrange_shared_legend(plot.list)
}

## ---- end-of-cond_covar