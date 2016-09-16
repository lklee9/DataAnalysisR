require(ggplot2)
require(ggrepel)
library(reshape2)
library(gridExtra)
library(grid)
source("visualisation.R")

SeparateByClass <- function(data.all) {
  data.class.factor <- factor(data.all[, length(data.all)])
  data.class.values <- levels(data.class.factor)
}

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

SeparateVisualisation <- function(data.all) {
  attributes.names <- GetAllAttributes(data.all)
  plot.list <- list()
  for (i in 1:length(attributes.names)) {
    plot <- SingleAttPlot(data.all, attributes.names[i])
    if (length(plot) > 1 || !is.na(plot)) {
      plot.list[[length(plot.list) + 1]] <- plot
    }
    else {
      print("Invalid plot at:")
      print(names(attributes.names)[i])
      print(plot)
    }
  }
  print("Plots Obtained")
  n <- length(plot.list)
  nCol <- floor(sqrt(n))
  grid_arrange_shared_legend(plot.list)
}

SingleAttPlot <- function(data.all, attribute.name) {
  attribute.data <- GetDataByAttribute(data.all, attribute.name)
  plot <- qplot(data = attribute.data, y = Distance, x = as.numeric(class_values), colour=class_values) + 
    #geom_text_repel(aes(label = class_values)) + 
    #theme(axis.text.x=element_text(angle=90, hjust = 1)) + 
    ggtitle(attribute.name)
  return(plot)
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
  
  #plot <- qplot(data = plot.data, x = class_value, y = attributes, colour = distance) +
    #geom_text_repel(aes(label = attributes)) + 
  #  ggtitle("Conditioned Covariate Drift")
  return(plot)
}