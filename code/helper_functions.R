# Libraries
library(data.table)
library(dplyr)
library(tictoc)
library(janitor)


# Helper functions

convert_to_csv <- function(filename) {
  filepath <- paste0("C:/Users/mvu02/Desktop/Projects/movie-metrics/data/unzipped/", filename)
  csv <- as.data.frame(fread(filepath, quote = ""))
  return(csv)
}