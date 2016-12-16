## ---- visualisation
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(circlize)


MDS <- function(distance_matrix, attributes) {
  fit <- cmdscale(distance_matrix, eig=TRUE, k=2)
  x <- fit$points[, 1]
  y <- fit$points[, 2]
  data.df <- data.frame(x, y)
  names(data.df) <- c('x', 'y')
  plot <- ggplot(data.df, aes(x, y)) +
    geom_point(color = 'red') +
    geom_text_repel(aes(label = attributes)) +
    ggtitle("Prior Drift 2 Attributes MDS")
  return(plot)
}

RawDataToHeatMap <- function(data.att1, data.att2, title, z_min = NA, z_max = NA) {
  # Construct Single attribute data
  attribute.list <- as.character(data.att1[, length(data.att1)])
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  data.mono <- data.frame(data.att1[, 1], attributes, stringsAsFactors = FALSE)
  
  # Construct Pairwise attibute data
  attribute.list <- as.character(data.att2[, length(data.att2)])
  attributes.1 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  attributes.2 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][2])
  data.dual <- data.frame(data.att2[, 1], attributes.1, attributes.2, stringsAsFactors = FALSE)
  
  if (is.na(z_min)) {
    z_min <- min(c(data.att1[,1], data.att2[,1]))
  }
  
  if (is.na(z_max)) {
    z_max <- max(c(data.att1[,1], data.att2[,1]))
  }
  return(HeatMap(data.mono, data.dual, title, z_min, z_max))
}

HeatMap <- function(data.mono, data.dual, title, z_min = 0, z_max = 1) {
  # Format of data 
  #
  #         data.mono                             data.dual
  # | distance | attributes |        | distance | attributes_1 | attributes_2 |
  #
  # Make Plot symmetrical
  attributes.first <- c(data.dual[,2], data.dual[,3], data.mono[,2])
  attributes.second <- c(data.dual[,3], data.dual[,2], data.mono[,2])
  
  # Add distances
  distances <- c(data.dual[,1], data.dual[,1], data.mono[,1])
  plot.data <- data.frame(distances, attributes.first, attributes.second)
  
  names(plot.data) <- c("distance", "attributes_1", "attributes_2")
  col <- colorRamp2(c(z_min, z_max), c("green", "red"))
  
  # Create Color Scale
  print(plot.data[, 1])
  vals <- unique(plot.data[, 1])
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric(c("green", "red"), domain = c(z_min, z_max))(vals)
  colz <- setNames(data.frame(vals[o], cols[o]), NULL)
  print(cols)
  print(colz)
  
  plot <- plot_ly(plot.data,
                  x = ~attributes_1, 
                  y = ~attributes_2, 
                  z = ~distance, 
                  zmin = z_min,
                  zmax = z_max,
                  #colors = colorRamp(c("green", "red")),
                  colorscale = colz,
                  width = 1000,
                  height = 750,
                  type = "heatmap")
  
  return(plot)
}

grid_arrange_shared_legend <- function(plots) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}


## ---- end-of-visualisation