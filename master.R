
#'===========================================================================================================
#' This is the master file
# it begins by installing and activating the required libraries, 
# it defines the working directory as this file's directory
# then calls
#  source("CB-TLTBI Functions.R")
#' Model_run.R
#' Inputs:
#' Then it calls CEA_analysis
#' 
#' Output:
#' This script outputs a table onto the clipboard that includes the results 
#' for each strategy (and its baseline) in separate rows.
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml
#'===========================================================================================================

package_list <- c("data.table", "tidyverse","lazyeval", "ggplot2", "rstudioapi","plyr", "tidyr","xlsx","RColorBrewer", "grid","scales","ggrepel", "egg", "cowplot", "gridExtra")
for (pack in package_list) {
  if (!requireNamespace(pack, quietly = TRUE)) {
    install.packages(pack)
  }
  library(pack, character.only = TRUE)
}



# Dynamically set working directory to the folder containing the script
this_file_path <- dirname(getActiveDocumentContext()$path)
setwd(this_file_path)

# input some functions

source("CB-TLTBI Functions.R")
# Load input reader

# Load inputs
inputs <- read_inputs("CEA_inputs_example.xlsx")

# View contents (for debugging)
str(inputs)

# Load scenario-specific files (example)
tb_incidence <- read.csv(inputs$file_paths$file_path[inputs$file_paths$data_type == "tb_incidence"])
age_dist <- read.csv(inputs$file_paths$file_path[inputs$file_paths$data_type == "age_distribution"])
#input parameter values
# this reads in an rds file and determines the seetting and persp ectives
source("Parameter values.R")
# prepare the data

source("CB-TLTBI_DataPreparation.R")

source("Model run.R")
source("CEA analysis.R")
source("ltbiutility figure.R")


