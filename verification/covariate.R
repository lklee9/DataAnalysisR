source("Verification/distribution_plots.R")

viewCovariate <- function(data1, data2) {
  plot.list <- list()
  for (i in 1:length(data1)) {
    plot <- singleHistogram(data1, data2, i)
    if (length(plot) > 1 || !is.na(plot)) {
      plot.list[[length(plot.list) + 1]] <- plot
    }
    else {
      print("Invalid plot at:")
      print(names(data1)[i])
      print(plot)
    }
  }
  print("Plots Obtained")
  grid_arrange_shared_legend(plot.list)
}
