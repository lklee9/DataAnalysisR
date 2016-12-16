source("read_data.R")
source("Verification/covariate.R")
source("Verification/posterior.R")

file1 <- "../../datasets/train_seed/20130505.arff"
file2 <- "../../datasets/train_seed/20131129.arff"

data.all <- ReadArffData(file1, file2)
viewPosterior(data.all[[1]], data.all[[2]])