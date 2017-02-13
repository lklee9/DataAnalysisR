library(ggplot2)
library(plotly)
source("./result_table.R")

PlotSamplingResultFiles <- function(file1, file2, file3) {
  results.striped <- StripData(file1, file2, file3)
  results.sd <- StripDataSd(file1, file2, file3)
  PlotSamplingResults(results.striped, results.sd)
}

PlotSamplingResults <- function(results.striped, results.sd) {
  subplot(
    plot_ly(results.striped, x = ~SampleSize, type = 'scatter', y = ~JointDistance, name = 'Joint Drift Distance', mode = 'lines', line = list(color = "blue")) %>%
      add_trace(x = c(4640, 4640), y= c(mean(results.striped$JointDistance)-0.05, mean(results.striped$JointDistance)+0.05), mode = "lines", name = "15625 Combinations", line = list(color = "red")) %>%
      add_markers(error_y = ~GetErrorValues(0.75, results.sd$JointDistance), showlegend = FALSE, marker = list(color = "black")) %>%
      layout(title = "Joint Drift Distance", annotations = list(text = "Joint Drift (4640 Combinations)", showarrow = FALSE, xref = "paper", yref = "paper", y = 1)),
    
    plot_ly(results.striped, x = ~SampleSize, type = 'scatter', y = ~CovariateDistance, name = 'Covariate Drift Distance', mode = 'lines', line = list(color = "green")) %>%
      add_trace(x = c(3125, 3125), y= c(mean(results.striped$CovariateDistance)-0.05, mean(results.striped$CovariateDistance)+0.05), mode = "lines", name = "3125 Combinations", line = list(color = "red")) %>%
      add_markers(error_y = ~GetErrorValues(0.5, results.sd$CovariateDistance), showlegend = FALSE, marker = list(color = "black")) %>%
      layout(title = "Covariate Drift Distance", annotations = list(text = "Covariate Drift (3125 Combinations)", showarrow = FALSE, xref = "paper", yref = "paper", y = 1)),
    
    plot_ly(results.striped, x = ~SampleSize, type = 'scatter', y = ~PosteriorDistance, name = 'Posteriror Drift Distance', mode = 'lines', line = list(color = "red")) %>%
      add_trace(x = c(4640, 4640), y= c(mean(results.striped$PosteriorDistance)-0.05, mean(results.striped$PosteriorDistance)+0.05), mode = "lines", name = "15625 Combinations", line = list(color = "red")) %>%
      add_markers(error_y = ~GetErrorValues(0.5, results.sd$PosteriorDistance), showlegend = FALSE, marker = list(color = "black")) %>%
      layout(title = "Posteriror Drift Distance", annotations = list(text = "Posterior Drift (4640 Combinations)", showarrow = FALSE, xref = "paper", yref = "paper", y = 1)),
    
    nrows = 3, shareX = TRUE
    ) %>%
    layout(title = "Drift of 0.5 on both covariates and class averaged on 20 tests", showlegend = FALSE)
}

GetErrorValues <- function(value.target, values.observed) {
  values.target <- rep(value.target, length(values.observed))
  #values.diff <- values.target - values.observed
  values.diff <- values.observed
  values.pos <- sapply(values.diff, function(x) if (x > 0) x else 0)
  values.neg <- sapply(values.diff, function(x) if (x < 0) abs(x) else 0)
  return(list(array = values.pos, arrayminus = values.pos, symmetric = FALSE, type = "data"))
}

StripData <- function(file1, file2, file3) {
  results1 <- ResultTable(file1)
  results2 <- ResultTable(file2)
  results3 <- ResultTable(file3)
  
  results.striped <- data.frame(results1[, 1], results1[, 2], results2[, 2], results3[, 2])
  
  names(results.striped) <- c("SampleSize", "JointDistance", "CovariateDistance", "PosteriorDistance")
  return(results.striped)
}

StripDataSd <- function(file1, file2, file3) {
  results1 <- ResultTable(file1)
  results2 <- ResultTable(file2)
  results3 <- ResultTable(file3)
  
  results.striped <- data.frame(results1[, 1], results1[, 4], results2[, 4], results3[, 4])
  
  names(results.striped) <- c("SampleSize", "JointDistance", "CovariateDistance", "PosteriorDistance")
  return(results.striped)
}