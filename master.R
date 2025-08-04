
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

package_list <- c("data.table", "tidyverse","lazyeval", "ggplot2", "rstudioapi")

for (pack in package_list) {
  if (!requireNamespace(pack, quietly = TRUE)) {
    install.packages(pack)
  }
  library(pack, character.only = TRUE)
}


this_file_path <- dirname(getActiveDocumentContext()$path)
setwd(this_file_path)



# Dynamically set working directory to the folder containing the script
this_file_path <- dirname(getActiveDocumentContext()$path)
setwd(this_file_path)



source("CB-TLTBI Functions.R")
source("Parameter values.R")

source("CB-TLTBI_DataPreparation.R")
source("Model run.R")
source("CEA analysis.R")
