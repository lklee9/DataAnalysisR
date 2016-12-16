require(ggplot2)
require(plotly)
#require(ggrepel)
#library(reshape2)
#library(gridExtra)
#library(grid)
source("visualisation.R")

## ---- posterior

VisualSingleAttributeStructure <- function(data.all, drift.type = "Likelihood") {
  attributes.conditioned <- unique(sapply(as.character(data.all$conditioned_value),
                                          function(x) strsplit(x, "=")[[1]][1]))
  plot.list <- lapply(attributes.conditioned, 
                      function(x) GetSinglePosteriorConditionPlot(data.all, x))
  names(plot.list) <- attributes.conditioned
  
  n <- length(plot.list)
  nCol <- ceiling(sqrt(n))
  g <- layout(subplot(plot.list, nrows = ceiling(n / nCol), shareX = FALSE, shareY = TRUE),
              showlegend = TRUE, title = "Detailed Posterior drift on 1 attribute", 
              margin = list(l=75, b =75, t = 75))
  return(g)
}

GetSinglePosteriorConditionPlot <- function(data.all, condition.attribute) {
  data.conditioned <- GetDataByConditionedAttributes(data.all, c(condition.attribute))
  condition.values <- sapply(as.character(data.conditioned$conditioned_value),
                             function(x) strsplit(x, "=")[[1]][2])
  posterior.drift <- data.conditioned$drift
  plot.data <- data.frame(condition.values, posterior.drift)
  #names(plot.data) <- c("attribute value", "drift magnitude")
  plot <- plot_ly(x = condition.values, 
                  y = posterior.drift,
                  name = condition.attribute,
                  type = "bar") %>% layout(title = "",
                                           xaxis = list(title = condition.attribute), 
                                           yaxis = list(title = "Posterior Drift"))
  return(plot)
}

VisualPairDetailedPosterior <- function(data.att.2) {
  z.min <- min(data.att.2$drift)
  z.max <- max(data.att.2$drift)
  attribute.all <- unique(sapply(as.character(data.att.2$attribute_subset),
                          function(x) strsplit(x, "[_=]")[[1]][1]))
  subplot.list <- lapply(attribute.all, 
                         function(x) 
                           layout(subplot(
                             lapply(attribute.all, function(y) 
                               MakeMatrixPlot(data.att.2, x, y, z.min, z.max)), 
                             nrows = 1, shareX = FALSE, shareY = TRUE),
                             showlegend = FALSE, title = "", 
                             margin = list(l=75, b =75, t = 75)))
  
  g <- layout(subplot(subplot.list, nrows = length(subplot.list), shareX = TRUE, shareY = FALSE),
              showlegend = FALSE, title = "Detailed Posterior drift on 2 attributes", 
              margin = list(l=75, b =75, t = 75))
  return(g)
}

MakeMatrixPlot <- function(data.all, attribute.1, attribute.2, z.min, z.max) {
  if (attribute.1 == attribute.2) {
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showtick = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    return(plot_ly(x = c('a'), y=c('a'), type='heatmap') %>% layout(xaxis = ax, yaxis = ax))
  }
  data.conditioned <- GetDataByConditionedAttributes(data.all, c(attribute.1, attribute.2))
  attribute.1.values <- unique(sapply(as.character(data.conditioned$conditioned_value), 
                                      function(x) strsplit(x, "[_=]")[[1]][2]))
  attribute.2.values <- unique(sapply(as.character(data.conditioned$conditioned_value),
                                      function(x) strsplit(x, "[_=]")[[1]][4]))
  value.matrix <- sapply(attribute.1.values, function(x) 
    sapply(attribute.2.values, function(y) 
      GetDriftByCondition(data.conditioned, attribute.1, x, attribute.2, y)))
  return(matrixToHeatmap(value.matrix, z.min, z.max,
                         c("", attribute.1, attribute.2)))
}

GetDriftByCondition <- function(data.conditioned, 
                                attribute.1, attribute.1.value, 
                                attribute.2, attribute.2.value) {
  condition.string <- paste0(attribute.1, "=", attribute.1.value, "_", attribute.2, "=", attribute.2.value)
  if (condition.string %in% data.conditioned$conditioned_value) {
    return(data.conditioned[data.conditioned$conditioned_value == condition.string, 
                            colnames(data.conditioned) == "drift"])
  }
  else {
    return(0.0)
  }
}

GetDataByConditionedAttributes <- function(data.all, conditioned.attributes) {
  selected.row <- sapply(data.all$conditioned_value, 
                         function(x) CheckConditionStringContainsAttributes(x, conditioned.attributes))
  return(data.all[selected.row, ])
}

CheckConditionStringContainsAttributes <- function(condition.string, attributes) {
  condition.separate <- strsplit(as.character(condition.string), "[_=]")[[1]]
  return(Reduce(function(x, y) x & y, attributes %in% condition.separate))
}

## ---- end-of-posterior