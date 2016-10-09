library(ggplot2)
library(plotly)
source("./result_table.R")

PlotSamplingResultFiles <- function(file1, file2, file3) {
  results.striped <- StripData(file1, file2, file3)
  PlotSamplingResults(results.striped)
}

PlotSamplingResults <- function(results.striped) {
  plot_ly(results.striped, x = ~SampleSize, type = 'scatter', y = ~JointDistance, name = 'Joint Drift Distance', mode = 'lines+markers') %>%
    add_trace(y = ~CovariateDistance, name = 'Covariate Drift Distance', mode = 'lines+markers') %>%
    add_trace(y = ~PosteriorDistance, name = 'Posterior Drift Distance', mode = 'lines+markers') %>%
    layout(title = "Drift of 0.5 on both covariates and class")
}

StripData <- function(file1, file2, file3) {
  results1 <- ResultTable(file1)
  results2 <- ResultTable(file2)
  results3 <- ResultTable(file3)
  
  results.striped <- data.frame(results1[, 1], results1[, 2], results2[, 2], results3[, 2])
  names(results.striped) <- c("SampleSize", "JointDistance", "CovariateDistance", "PosteriorDistance")
  return(results.striped)
}