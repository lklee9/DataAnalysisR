source("distance_matrix.R")
source("visualisation.R")
source("result_table.R")


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

runVisualisation(file_path)