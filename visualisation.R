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

SingleHistogramVisual <- function(data.all) {
  attribute.list <- as.character(data.all[, length(data.all) - 1])
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  distances <- data.all[, 1]
  plot.data <- data.frame(attributes, distances)
  names(plot.data) <- c("attributes", "distance")
  
  plot <- ggplot(plot.data, aes(x = attributes, y = distance, fill = attributes)) + 
    geom_bar(colour = "black", width = 0.9, alpha = 1, stat = "identity") +
    guides(fill=FALSE) +
    ggtitle("Prior Drift")
  return(plot)
}

DualHistogramVisual <- function(data.all) {
  attribute.list <- as.character(data.all[, length(data.all) - 1])
  attributes.1 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  attributes.2 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][2])
  attributes.first <- c(attributes.1, attributes.2)
  attributes.second <- c(attributes.2, attributes.1)
  attributes.unique <- unique(attributes.first)
  attributes.first <- c(attributes.first, attributes.unique)
  attributes.second <- c(attributes.second, attributes.unique)
  distances <- data.all[, 1]
  distances <- c(distances, distances, rep(0, length(attributes.unique)))
  plot.data <- data.frame(distances, attributes.first, attributes.second)
  names(plot.data) <- c("distance", "attributes_1", "attributes_2")
  
  # To change the color of the gradation :
  plot <- ggplot(plot.data, aes(attributes_1, attributes_2, z= distance)) + 
    geom_tile(aes(fill = distance)) + 
    theme_bw() + 
    #scale_fill_gradient(low="green", high="red", limits=c(0,1)) +
    scale_fill_gradient(low="green", high="red") +
    ggtitle("Prior Drift 2 Attributes")
  
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