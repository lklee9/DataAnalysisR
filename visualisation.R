require(ggplot2)

## ---- visualisation

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

RawDataToHeatMap <- function(data.att1, data.att2, title) {
  # Construct Single attribute data
  attribute.list <- as.character(data.att1[, length(data.att1) - 1])
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  data.mono <- data.frame(data.att1[, 1], attributes, stringsAsFactors = FALSE)
  
  # Construct Pairwise attibute data
  attribute.list <- as.character(data.att2[, length(data.att2) - 1])
  attributes.1 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  attributes.2 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][2])
  data.dual <- data.frame(data.att2[, 1], attributes.1, attributes.2, stringsAsFactors = FALSE)
  
  return(HeatMap(data.mono, data.dual, title))
}

HeatMap <- function(data.mono, data.dual, title) {
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
  # To change the color of the gradation :
  plot <- ggplot(plot.data, aes(attributes_1, attributes_2, z= distance)) + 
    geom_tile(aes(fill = distance)) + 
    theme_bw() + 
    #scale_fill_gradient(low="green", high="red", limits=c(0,1)) +
    scale_fill_gradient(low="green", high="red") +
    ggtitle(title)
  
  return(plot)
}

grid_arrange_shared_legend <- function(plots) {
  n <- length(plots)
  nCol <- floor(sqrt(n))
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