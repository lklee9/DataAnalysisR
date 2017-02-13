source("visualisation.R")
source("visualise_results/heatmaps.R")
library(ggplot2)
library(plotly)

compareResults <- function(result.table.list.1, result.table.list.2) {
  attribute.subset.length <- length(result.table.list.1)
  if (attribute.subset.length == 1) {
    
  }
  else if (attribute.subset.length == 2) {
    z.min <- min(min(result.table.list.1[[1]]$drift), min(result.table.list.1[[1]]$drift))
    z.max <- max(max(result.table.list.1[[2]]$drift), max(result.table.list.1[[2]]$drift))
    plot.1 <- VisualPairAttributes(result.table.list.1[[1]], result.table.list.1[[2]],
                                   z.min = z.min, z.max = z.max)
    plot.2 <- VisualPairAttributes(result.table.list.2[[1]], result.table.list.2[[2]],
                                   z.min = z.min, z.max = z.max)
    g <- layout(subplot(list(plot.1, plot.2), nrows = 1, shareX = TRUE, shareY = TRUE),
                showlegend = FALSE, title = "Comparing")
    return(g)
  }
  else {
    stop("error")
  }
}