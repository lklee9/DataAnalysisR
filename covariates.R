source("./visualisation.R")
## ---- covar

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

DualHistogramVisual <- function(data.att1, data.att2, title = "Prior") {
  RawDataToHeatMap(data.att1, data.att2, paste(title, "Drift 2 Attributes"))
}

TripleCovariateVisual <- function(data.att1, data.att2, data.att3, title = "Prior") {
  # Get Attribute with max variance/sd
  variance.max <- max(data.att1[, 3])
  attribute.focus <- strsplit(as.character(data.att1[data.att1[, 3] == variance.max, length(data.att1) - 1]), "_")[[1]][1]
  
  data.att3 <- FilterDataAttribute(data.att3, attribute.focus)
  data.att2 <- FilterDataAttribute(data.att2, attribute.focus)
  
  return(RawDataToHeatMap(data.att2, data.att3, paste(title, "Drift 3 Attributes on ", attribute.focus)))
}

FilterDataAttribute <- function(data.all, attribute.target) {
  # Filter data containing max variance attribute
  tuple.indices <- grep(attribute.target, data.all[, length(data.all) - 1])
  data.filtered <- data.all[tuple.indices, ]
  # Remove focus attribute from attribute column
  data.attributes <- as.character(data.filtered[, length(data.filtered) - 1])
  data.attributes <- sapply(data.attributes, function(x) RemoveAttributeFromSet(x, attribute.target))
  data.filtered[, length(data.filtered) - 1] <- data.attributes
  return(data.filtered)
}

RemoveAttributeFromSet <- function(attribute.set, attribute.remove) {
  attribute.separated <- strsplit(attribute.set, "_")[[1]]
  attribute.clean <- attribute.separated[attribute.separated != attribute.remove]
  return(paste0(attribute.clean, collapse = "_"))
}

## ---- end-of-covar