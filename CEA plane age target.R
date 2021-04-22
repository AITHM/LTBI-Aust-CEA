#'===========================================================================================================
#' This script runs several analyses, one after the other
#' with different age group targets
#' and puts the results into a large table that can be pasted 
#' into excel. This can then be used to create nice tables 
#' for the main text and supplement.
#' 
#' Inputs:
#' Need to run the "Results table.R" first, which creates a couple of rds files that this code then uses.
#' 
#' Output:
#' rds files that can be used for creating the figure (CEA plane age target figure onshore)
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml

#' LOAD LIBRARIES ===========================================================================================
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)

#' This prevents the model run script (Model run.R) from sourcing
#' the "parameter values" script for the parameter values, because these
#' are, instead, defined below.
parameters.already.set <- 1

#' This makes sure the Data/Outputs folder is empty before running the script
filenames <- list.files("Data/Output", 
                        pattern = "*.rds", full.names = TRUE)
file.remove(filenames)

#' read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
params <- readRDS("params onshore.rds")
onshore <- 1

# params <- readRDS("params offshore.rds")
# onshore <- 0
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

#' WITH OR WITHOUT EMIGRATION #######CHANGE IN PARAMETER FILE TOO!!!!!!###########
withemig <- 0

#' Create a datatable that contains all of the combinations
#' of targets, i.e. by age and TB incidence in country of birth

#' Define age target
lower.age.targets <- c(10, 19, 29, 39, 59, 10, 10, 35)
if (onshore == 1) {
  tbincid.targets <- c("200+")
} else if (onshore == 0) {
  tbincid.targets <- c("100+")
}
target.dt<- expand.grid(lower.age.targets, tbincid.targets)
target.dt <- as.data.table(target.dt)
setnames(target.dt, "Var1", "age.low")
setnames(target.dt, "Var2", "tbincid")
target.dt[, age.high := age.low + 11]
target.dt[age.low == 35, age.high := 66]
target.dt[age.low == 10, age.high := 20]
target.dt[age.low == 39, age.high := 60]
target.dt[6, age.high := 36]
target.dt[7, age.high := 66]

#' The following loops down the rows of the table
#' and runs the model with each specified target
#' Then the output is analysed and entered
#' into a new enormous data table.

for(target.x in 1:nrow(target.dt)) {
  
  source("CB-TLTBI Functions.R")
  source("Parameter values.R")
  
  #' Define target: TB incidence in country of birth: "40+" "100+" "150+" "200+"
  target.tbincid <- target.dt[target.x, tbincid]
  
  #' Define age target
  age.limit.older.than <- target.dt[target.x, age.low]
  age.limit.younger.than <- target.dt[target.x, age.high]
  
  #' Function that defines target population for model run
  Get.POP <- function(DT, strategy) {
    
    if (target.tbincid == "200+") {
      (ifelse(DT[, ISO3] == "200+", 1, 0)) &
        (ifelse(DT[, AGERP] > age.limit.older.than, 1, 0) &
           ifelse(DT[, AGERP] < age.limit.younger.than, 1, 0))
    } else if (target.tbincid == "150+") {
      (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) &
        (ifelse(DT[, AGERP] > age.limit.older.than, 1, 0) &
           ifelse(DT[, AGERP] < age.limit.younger.than, 1, 0))
    } else if (target.tbincid == "100+") {
      (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) |
         ifelse(DT[, ISO3] == "100-149", 1, 0)) &
        (ifelse(DT[, AGERP] > age.limit.older.than, 1, 0) &
           ifelse(DT[, AGERP] < age.limit.younger.than, 1, 0))
    } else if (target.tbincid == "40+") {
      (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) |
         ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
        (ifelse(DT[, AGERP] > age.limit.older.than, 1, 0) &
           ifelse(DT[, AGERP] < age.limit.younger.than, 1, 0))
    }
  }
  
  #' Run the data prep
  source("CB-TLTBI_DataPreparation.R")
  
  #' Run the model
  source("Model run.R")
  
  #' Run the analysis file to sort the 
  #' model output and put the main findings 
  #' into a table called "table1"
  source("CEA analysis.R")
  
  #' Add some intial columns in table1
  #' that specifies the target group
  table1 <- data.table(rep(target.tbincid, nrow(table1)), table1)
  table1 <- data.table(rep(age.limit.younger.than - 1, nrow(table1)), table1)
  table1 <- data.table(rep(age.limit.older.than + 1, nrow(table1)), table1)
  setnames(table1, 1, "age.low")
  setnames(table1, 2, "age.high")
  setnames(table1, 3, "tbincid")
  
  #' Bind the results from each model run together
  #' into one large table
  if (target.x == 1) {
    results.dt <- copy(table1)
  } else {
    results.dt <- rbind(results.dt, table1)
  }
  
  #' remove the files in the output folder
  file.remove(filenames)
  
}

#' Save the output to file
if (onshore == 1 & withemig == 1) {
  saveRDS(results.dt, file = "Data/agetargetonshore.rds")
} else if (onshore == 1 & withemig == 0) {
  saveRDS(results.dt, file = "Data/agetargetonshorenoemig.rds")
} else if (onshore == 0 & withemig == 1) {
  saveRDS(results.dt, file = "Data/agetargetoffshore.rds")
} else if (onshore == 0 & withemig == 0) {
  saveRDS(results.dt, file = "Data/agetargetoffshorenoemig.rds")
}

#' Write the table to clipboard so I can paste it into Excel
write.table(results.dt, file = "clipboard-16384", sep = "\t", row.names = FALSE)
