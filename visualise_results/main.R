source("visualise_results/result_table.R")
source("visualise_results/heatmaps.R")

file.1 <- "../data_out/FrequencyMaps/20130419_20130505/20130419_20130505_1-attributes_covariate.csv"
file.2 <- "../data_out/FrequencyMaps/20130419_20130505/20130419_20130505_2-attributes_covariate.csv"
file.3 <- "../data_out/FrequencyMaps/20130419_20130505/20130419_20130505_3-attributes_covariate.csv"

table.1 <- ResultTable(file.1)
table.2 <- ResultTable(file.2)
table.3 <- ResultTable(file.3)

print(VisualSingleAttribute(table.1))
print(VisualPairAttributes(table.1, table.2))
print(VisualTripleAttributes(table.1, table.2, table.3))