#source("./visualisation.R")
library(ggplot2)
library(plotly)
## ---- heatmaps

VisualSingleAttribute <- function(data.all, drift.type = "Prior") {
  attribute.list <- as.character(data.all$attribute_subset)
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  distances <- data.all$drift
  plot.data <- data.frame(attributes, distances)
  names(plot.data) <- c("attributes", "distance")
  
  plot <- ggplot(plot.data, aes(x = attributes, y = distance, fill = attributes)) + 
    geom_bar(colour = "black", width = 0.9, alpha = 1, stat = "identity") +
    guides(fill=FALSE) +
    ggtitle(paste0(drift.type, " Drift"))
  return(plot)
}

VisualPairAttributes <- function(data.att1, data.att2, 
                                 z.min = NA, z.max = NA, 
                                 drift.type = "Prior", size = c(620, 510),
                                 title = "Drift 2 Attributes") {
  values.matrix <- PairDataToMatrix(data.att1, data.att2)
  z.min <- if (is.na(z.min)) min(values.matrix) else z.min
  z.max <- if (is.na(z.max)) max(values.matrix) else z.max
  return(matrixToHeatmap(values.matrix, 
                         z.min, z.max, size = size,
                         labels = c(paste(drift.type, title, sep = " "), 
                                    "attributes", "attributes")))
}

VisualTripleAttributes <- function(data.att1, data.att2, data.att3, 
                                   z.min = NA, z.max = NA,
                                   drift.type = "Prior", size = c(620, 510)) {
  # Get Attribute with max variance/sd
  variance.max <- max(data.att1$sd)
  attribute.focus <- strsplit(as.character(data.att1[data.att1$sd==variance.max, 
                                                     colnames(data.att1) == "attribute_subset"]), "_")[[1]][1]
  
  data.att1 <- FilterDataAttribute(data.att2, attribute.focus)
  data.att2 <- FilterDataAttribute(data.att3, attribute.focus)
  
  return(VisualPairAttributes(data.att1, data.att2, 
                              z.min, z.max, drift.type, size, 
                              paste0("2 Attribute Drift on ", attribute.focus)))
}

PairDataToMatrix <- function(data.att1, data.att2) {
  # Construct Single attribute data
  attribute.list <- as.character(data.att1$attribute_subset)
  attributes <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  data.mono <- data.frame(data.att1$drift, attributes, stringsAsFactors = FALSE)
  
  # Construct Pairwise attibute data
  attribute.list <- as.character(data.att2$attribute_subset)
  attributes.1 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][1])
  attributes.2 <- sapply(attribute.list, function(x) strsplit(x, "_")[[1]][2])
  data.dual <- data.frame(data.att2$drift, attributes.1, attributes.2, stringsAsFactors = FALSE)
  
  # Make Plot symmetrical
  attributes.first <- c(data.dual[,2], data.dual[,3], data.mono[,2])
  attributes.second <- c(data.dual[,3], data.dual[,2], data.mono[,2])
  
  # Add distances
  distances <- c(data.dual[,1], data.dual[,1], data.mono[,1])
  plot.data <- data.frame(distances, attributes.first, attributes.second)
  attributes.all <- unique(c(attributes, attributes.1, attributes.2))
  
  value.matrix <- sapply(seq(1:length(attributes.all)),
                         function(x) 
                           sapply(seq(1:length(attributes.all)), 
                                  function(y) 
                                    plot.data[plot.data[, 2] == attributes.all[x] &
                                                plot.data[, 3] == attributes.all[y], 1]))
  colnames(value.matrix) <- attributes.all
  row.names(value.matrix) <- attributes.all
  return(value.matrix)
}

matrixToHeatmap <- function(values.matrix, z_min, z_max, labels = c("", "", ""), size = c(620, 510)) {
  # Create Color Scale
  vals <- unique(scales::rescale(c(values.matrix, z_min, z_max)))
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric(c("green", "red"), domain = NULL)(vals)
  colz <- setNames(data.frame(vals[o], cols[o]), NULL)
  
  plot <- plot_ly(x = colnames(values.matrix), 
                  y = rownames(values.matrix), 
                  z = values.matrix, 
                  zmin = z_min,
                  zmax = z_max,
                  colorscale = colz, 
                  type = "heatmap") %>% 
    layout(margin = list(l = 100, r = 30, b = 100, t = 50, pad = 10),
           width = size[1],
           height = size[2],
           title = labels[1], 
           xaxis = list(title = labels[2]), 
           yaxis = list(title = labels[3]))
           
  
  return(plot)
}

FilterDataAttribute <- function(data.all, attribute.target) {
  # Filter data containing max variance attribute
  tuple.indices <- grep(attribute.target, data.all$attribute_subset)
  data.filtered <- data.all[tuple.indices, ]
  # Remove focus attribute from attribute column
  data.attributes <- as.character(data.filtered$attribute_subset)
  data.attributes <- sapply(data.attributes, function(x) RemoveAttributeFromSet(x, attribute.target))
  data.filtered$attribute_subset <- data.attributes
  return(data.filtered)
}

RemoveAttributeFromSet <- function(attribute.set, attribute.remove) {
  attribute.separated <- strsplit(attribute.set, "_")[[1]]
  attribute.clean <- attribute.separated[attribute.separated != attribute.remove]
  return(paste0(attribute.clean, collapse = "_"))
}

## ---- end-of-heatmaps