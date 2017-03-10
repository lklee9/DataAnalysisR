source("visualise_results/mds.R")
source("visualisation.R")
source("visualise_results/result_table.R")
source("verification/read_data.R")
source("verification/compare_distributions.R")
library(knitr)  


#file_path = "../data_out/martvard/airlines_4-attributes_prior.csv"
file.1 = "../data_out/FrequencyMaps/20130505_20131129/20130505_20131129_1-attributes_covariate.csv"
file.2 = "../data_out/FrequencyMaps/20130505_20131129/20130505_20131129_2-attributes_covariate.csv"
#file_path = "../data_out/martvard/elecNormNew_4-attributes_prior.csv"
#file_path = "../data_out/martvard/sensor_4-attributes_prior.csv"

runVisualisation <- function(file.1, file.2) {
  result.1 <- ResultTable(file.1)
  result.2 <- ResultTable(file.2)
  DualHistogramVisual(file.1, file.2)
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
  rmarkdown::render("Notebook/SatelliteResults.Rmd", params = params.all, output_file=paste0(title, '.html'), output_dir = "Notebook/Satellite")
}

RenderAll <- function() {
  RenderHTML("20130419", "20131129")
  RenderHTML("20130505", "20131129")
  RenderHTML("20130606", "20131129")
  RenderHTML("20130708", "20131129")
  RenderHTML("20130910", "20131129")
  RenderHTML("20131113", "20131129")
}

#file1 <- "../datasets/train_seed/20130505.arff"
#file2 <- "../datasets/train_seed/20131129.arff"
#runVisualisation(file.1, file.2)
##RenderHTML("20130505", "20131129")

RenderDriftTimeline <- function(folder, subset.length) {
  params.all <- list()
  params.all$result.folder <- folder
  params.all$subset.length <- subset.length

  rmarkdown::render("Notebook/DriftTimeline.Rmd", params = params.all, 
                    output_file=paste0(folder, "_", subset.length, '.html'), 
                    output_dir = "Notebook/DriftTimeline")
}


