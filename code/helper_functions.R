# Libraries
library(data.table)
library(dplyr)
library(tictoc)
library(janitor)
library(stringr)


# Helper functions

convert_to_csv <- function(filename) {
  filepath <- paste0("C:/Users/mvu02/Downloads/", filename, "/data.tsv")
  csv <- as.data.frame(fread(filepath, quote = ""))
  return(csv)
}