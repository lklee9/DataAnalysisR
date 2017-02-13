require(ggplot2)
require(ggrepel)
library(reshape2)
library(gridExtra)
library(grid)
#source("visualisation.R")
#source("visualise_results/heatmaps.R")

## ---- likelihood

GetDataByClass <- function(data.all, class.value) {
  row.selected <- sapply(strsplit(as.character(data.all$conditioned_value), "="),
                         function(x) x[2] == class.value)
  return(data.all[row.selected, ])
}

VisualSingleLikelihoodAttribute <- function(data.all) {
  distance <- data.all$drift
  class_value <- sapply(as.character(data.all$conditioned_value), 
                        function(x) strsplit(x, "=")[[1]][2])
  attributes <- sapply(as.character(data.all$attribute_subset), 
                       function(x) strsplit(x, "_")[[1]][1])
  
  plot.data <- data.frame(distance, class_value, attributes)
  names(plot.data) <- c("distance", "class_value", "attributes")
    
  plot <- plot_ly(plot.data,
                  x = ~class_value, 
                  y = ~attributes, 
                  z = ~distance, 
                  colors = colorRamp(c("green", "red")),
                  width = 620,
                  height = 510,
                  type = "heatmap")
  
  return(plot)
}

VisualPairLikelihoodAttributes <- function(data.att1, data.att2) {
  class.all <- unique(sapply(as.character(data.att1$conditioned_value), 
                             function(x) strsplit(x, "=")[[1]][2]))
  plot.list <- list()
  z_min <- min(data.att1$drift)
  z_max <- max(data.att2$drift)
  for (i in 1:length(class.all)) {
    data.mono <- GetDataByClass(data.att1, class.all[i])
    data.dual <- GetDataByClass(data.att2, class.all[i])
    plot <- VisualPairAttributes(data.mono, data.dual, z_min, z_max, 
                                 drift.type = "Likelihood", title = class.all[i], size = c(520, 510))
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
  g <- layout(subplot(plot.list, nrows = ceiling(n / nCol), shareX = TRUE, shareY = TRUE),
              showlegend = FALSE, title = "Detailed Likelihood drift on 2 attributes", 
              margin = list(l=75, b =75, t = 75)) %>% 
    layout(
      annotations = list(
      list(x = 0.15 , y = 1.025, text = class.all[1], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.50 , y = 1.025, text = class.all[2], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.85 , y = 1.025, text = class.all[3], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.15 , y = 0.66, text = class.all[4], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.50 , y = 0.66, text = class.all[5], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.85 , y = 0.66, text = class.all[6], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.15 , y = 0.315, text = class.all[7], showarrow = F, xref='paper', yref='paper'),
      list(x = 0.50 , y = 0.315, text = class.all[8], showarrow = F, xref='paper', yref='paper'))
    )
  return(g)
}

## ---- end-of-likelihood