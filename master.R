
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


package_list <- c("data.table", "tidyverse","lazyeval", "ggplot2", "rstudioapi","plyr", "tidyr","xlsx","RColorBrewer", "grid","scales","ggrepel", "egg", "cowplot", "gridExtra", "openxlsx", "readxl", "dplyr")
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


# Load all parameter sheets
param_file <- "parameters.xlsx"

# Main parameter table (e.g., for PSA)
cascade_params <- read_excel(param_file, sheet = "cascade_of_care")

# Clean up name column if needed
cascade_params$p <- trimws(cascade_params$p)

# Load other sheets
utilities <- read_excel(param_file, sheet = "utilities")  # <- you may want to rename this to 'utilities'
costs <- read_excel(param_file, sheet = "costs")
age_dist <- read_excel(param_file, sheet = "age_dist")
tb_incidence <- read_excel(param_file, sheet = "tb_incidence")





#input parameter values
# this reads in an rds file and determines the seetting and persp ectives
#source("Parameter values.R")
source("parameter_values.R")
# prepare the data

switches <- read_excel(param_file, sheet = "switches")

get_switch <- function(name) {
  val <- switches$value[switches$name == name]
  if (length(val) == 0) stop(paste("Switch not found:", name))
  return(val)
}

# Use the switches
onshore <- as.numeric(get_switch("onshore"))
emigration <- as.numeric(get_switch("emigration"))
disc <- as.numeric(get_switch("disc"))
startyear <- as.numeric(get_switch("startyear"))
totalcycles <- as.numeric(get_switch("totalcycles"))
finalyear <- startyear + totalcycles
kill.off.above <- as.numeric(get_switch("kill.off.above"))

# Convert string lists
testlist <- strsplit(get_switch("testlist"), ",")[[1]]
treatmentlist <- strsplit(get_switch("treatmentlist"), ",")[[1]]




source("CB-TLTBI_DataPreparation.R")

source("model_run.R")
source("cea_analysis.R")
source("ltbiutility figure.R")


