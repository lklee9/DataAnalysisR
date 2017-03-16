library(plotly)
file.pos <- "../data_out/synthetic_5Att_5Val/n1000000_m0.7_posterior/stream/POSTERIOR_5000.csv"

## ---- drift_timeline

PlotAllWindowSizes <- function(drift.type, subset.length, directory, column.indcies, ymax = NA, x.raw.values = NA) {
  data.list <- GetTimelineData(drift.type, subset.length, directory)
  window.sizes <- GetWindowSize(drift.type, subset.length, directory)
  if (length(column.indcies) == 0) column.indcies <- (2:(ncol(data.list[[1]])))
  if (subset.length < GetMaxSubsetLength(directory)) {
      max.data.list <- GetTimelineData(drift.type, GetMaxSubsetLength(directory), directory)
      data.list <- lapply(seq(1:length(data.list)), function(x) data.frame(
          max.data.list[[x]][,c(1,2)], data.list[[x]][, unique(c(column.indcies))]))
  }
  plot.list <- lapply(seq(1:length(data.list)), 
                      function(x) 
                        PlotDriftTimeline(data.list[[x]], window.sizes[x], 900, 300 * length(data.list), ymax, x.raw.values))
  
  g <- layout(subplot(plot.list, nrows = length(plot.list), shareX = TRUE, shareY = TRUE),
              showlegend = TRUE, 
              title = paste(drift.type)) %>%
    layout(
      annotations = list(
      list(x = 1.235 , y = 1.08, text = "Window Sizes - Attribute", showarrow = F, xref='paper', yref='paper')),
      autosize = T)
      
  return(g)
}

PlotWindowSize <- function(drift.type, window.length, subset.lengths, directory, ymax = NA, x.raw.values = NA) {
  data.list <- lapply(subset.lengths, function(x) GetTimelineData(drift.type, x, directory, window.length))
  result.table <- Reduce(function(x, y) merge.data.frame(x, y, by = "points"), data.list)
  if (length(result.table) == 1) {
    result.table <- result.table[[1]]
  }
  
  timeline.plot <- PlotDriftTimeline(result.table, window.length, 900, 300, ymax, x.raw.values) %>%
    layout(showlegend = TRUE, title = paste(drift.type, window.length)) %>%
    layout(
      annotations = list(
      list(x = 1.235 , y = 1.08, text = "Window Sizes - Attribute", showarrow = F, xref='paper', yref='paper')))
      
  return(timeline.plot)
}

PlotDriftTimeline <- function(drift.timeline, window.size, width, height, ymax, x.raw.values) {
  if (is.na(ymax)) {
    if (ncol(drift.timeline) == 2) {
      ymax <- max(drift.timeline[, 2])
      }
    else {
      ymax <- max(drift.timeline[, seq(2, ncol(drift.timeline))])
    }
  }
  if (!is.na(x.raw.values)) {
    drift.timeline[, 1] <- x.raw.values[drift.timeline[, 1]]
  }
  col.names <- names(drift.timeline)
  p <- plot_ly(drift.timeline, x = ~points, y = drift.timeline[, 2], width = width, height = height,
               name = paste(window.size, names(drift.timeline)[2]), type = "scatter", mode = "lines") %>%
  #p <- plot_ly(drift.timeline, x = ~points, y = drift.timeline[, 2],
  #             name = paste(window.size, names(drift.timeline)[2]), type = "scatter", mode = "lines") %>%
     layout(yaxis = list(range = c(0,ymax)))
  if (ncol(drift.timeline) > 2) {
    for (i in 3:ncol(drift.timeline)) {
      p <- add_trace(p = p, y = drift.timeline[, i], 
                     name = paste(window.size, names(drift.timeline)[i]), mode = 'marker+lines')
    }
  }
  return(p)
}

GetTimelineData <- function(drift.type, subset.length, directory, window.size = ".*") {
    files.all <- list.files(path = directory, pattern = paste0(drift.type, "_", window.size, "_", subset.length, ".csv"))
    print(files.all)
    window.sizes <- sapply(files.all, function(x) as.integer(strsplit(x, "[_.]")[[1]][2]))
    files.all <- files.all[order(window.sizes, files.all)]
    window.sizes <- sort(window.sizes)
  
    data.list <- lapply(files.all, function(x) read.csv(paste0(directory, "/", x)))
    if (subset.length == GetMaxSubsetLength(directory)) {
        data.list <- lapply(data.list, setNames, c("points", "all_attributes"))
    }
    return(data.list)
}

GetWindowSize <- function(drift.type, subset.length, directory) {
    files.all <- list.files(path = directory, pattern = paste0(drift.type, "_.*_", subset.length))
    window.sizes <- sapply(files.all, function(x) as.integer(strsplit(x, "[_.]")[[1]][2]))
    window.sizes <- sort(window.sizes)
    return(window.sizes)
}

GetMaxSubsetLength <- function(directory) {
    all.files <- list.files(path = directory, full.names = FALSE)
    all.lengths <- sapply(all.files, function(x) as.numeric(strsplit(strsplit(x, split = "_")[[1]][3], split = "\\.")[[1]][1]))
    return(max(all.lengths))
}

## ---- end-of-drift_timeline