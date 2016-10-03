source("distance_matrix.R")
source("visualisation.R")
source("result_table.R")
source("read_data.R")
source("compare_distributions.R")
library(knitr)  


#file_path = "../data_out/martvard/airlines_4-attributes_prior.csv"
file_path = "../data_out/martvard/20130622_20131129_4-attributes_prior.csv"
#file_path = "../data_out/martvard/elecNormNew_4-attributes_prior.csv"
#file_path = "../data_out/martvard/sensor_4-attributes_prior.csv"

runVisualisation <- function(file_path) {
  resultTable <- ResultTable(file_path)
  attributes.all <- ExtractAttributes(resultTable)
  distanceMatrix <- DistanceMatrix(resultTable)
  MDS(distanceMatrix, attributes.all)
}

require(foreign)
HistogramAnalysis <- function(file1, file2=NA){
  data.all <- ReadArffData(file1, file2)
  Histogram(data.all[[1]], data.all[[2]])
}

RenderHTML <- function(file1, file2) {
  params.all <- list()
  params.all$file1 <- file1
  params.all$file2 <- file2
  title <- paste0(file1, "_", file2)
  rmarkdown::render("Notebook/SatelliteResults.Rmd", params = params.all, output_file=paste0(title, '.html'), output_dir = "Notebook/Satellite/")
}

RenderAll <- function() {
  RenderHTML("20130419", "20131129")
  RenderHTML("20130505", "20131129")
  RenderHTML("20130606", "20131129")
  RenderHTML("20130708", "20131129")
  RenderHTML("20130910", "20131129")
  RenderHTML("20131113", "20131129")
}

file1 <- "../datasets/train_seed/20130505.arff"
file2 <- "../datasets/train_seed/20131129.arff"