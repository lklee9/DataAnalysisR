library(plotly)
file.pos <- "../data_out/synthetic_5Att_5Val/n1000000_m0.7_posterior/stream/POSTERIOR_5000.csv"

PlotAllWindowSizes <- function(drift.type, subset.length, directory, column.indcies) {
  files.all <- list.files(path = directory, pattern = paste0(drift.type, "_.*_", subset.length))
  window.sizes <- sapply(files.all, function(x) as.integer(strsplit(x, "[_.]")[[1]][2]))
  files.all <- files.all[order(window.sizes, files.all)]
  window.sizes <- sort(window.sizes)
  
  print(files.all)
  data.list <- lapply(files.all, function(x) GetDriftTimeline(paste0(directory, "/", x)))
  plot.list <- lapply(seq(1:length(data.list)), 
                      function(x) 
                        PlotDriftTimeline(data.list[[x]][, unique(c(1,2,column.indcies))], window.sizes[x]))
  
  g <- layout(subplot(plot.list, nrows = length(plot.list), shareX = TRUE, shareY = TRUE),
              showlegend = TRUE, 
              title = paste(drift.type)) %>%
    layout(
      annotations = list(
      list(x = 1.14 , y = 1.03, text = "Window Sizes", showarrow = F, xref='paper', yref='paper')))
      
  return(g)
}

PlotDriftTimeline <- function(drift.timeline, window.size) {
  col.names <- names(drift.timeline)
  p <- plot_ly(drift.timeline, x = ~points, y = drift.timeline[, 2], 
               name = paste(window.size, names(drift.timeline)[2]), type = "scatter", mode = "lines")
  if (ncol(drift.timeline) > 2) {
    for (i in 3:ncol(drift.timeline)) {
      p <- add_trace(p = p, y = drift.timeline[, i], 
                     name = paste(window.size, names(drift.timeline)[i]), mode = 'marker+lines')
    }
  }
  return(p)
}

GetDriftTimeline <- function(file.name) {
  return(read.csv(file.name))
}