source("distance_matrix.R")
source("visualisation.R")
source("result_table.R")
source("read_data.R")


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

file1 <- "../datasets/train_seed/20130419.arff"
file2 <- "../datasets/train_seed/20131129.arff"