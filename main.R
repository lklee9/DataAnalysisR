source("distance_matrix.R")
source("visualisation.R")
source("result_table.R")


file_path = "../data_out/martvard/airlines_2-attributes_prior.csv"
resultTable <- ResultTable(file_path)
attributes.all <- ExtractAttributes(resultTable)
distanceMatrix <- DistanceMatrix(resultTable)
MDS(distanceMatrix, attributes.all)
