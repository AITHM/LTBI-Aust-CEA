
# This script runs several analyses, one after the other
# and puts the results into a large table that can be pasted 
# into excel. This can then be used to create nice tables 
# for the main text and supplement.

library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)

parameters.already.set <- 1

# read in parameter list and values, which is  defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
#setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
params <- readRDS("params onshore.rds")
# params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

# Create a datatable that contains all of the combinations
# of targets, i.e. by age and TB incidence in country of birth

# Define age target
lower.age.targets <- c(10)
upper.age.targets <- c(36, 66)
tbincid.targets <- c("40+", "100+", "150+", "200+")
target.dt<- expand.grid(lower.age.targets, upper.age.targets, tbincid.targets)
target.dt <- as.data.table(target.dt)
setnames(target.dt, "Var1", "age.low")
setnames(target.dt, "Var2", "age.high")
setnames(target.dt, "Var3", "tbincid")
target.dt[age.low == 35 & age.high == 36, kick := 1]
target.dt <- subset(target.dt, is.na(kick))
target.dt[, tbincid := as.character(tbincid)]
target.dt[, kick := NULL]

# The following loops down the rows of the table
# and runs the model with each specified target
# Then the output is analysed and entered
# into a new enormous data table.

# Testing:
# target.x <- 3
# target.dt <- target.dt[1:3,]

for(target.x in 1:nrow(target.dt)) {
  
  source("CB-TLTBI Functions.R")
  source("Parameter values.R")

  # Define target: TB incidence in country of birth: "40+" "100+" "150+" "200+"
  target.tbincid <- target.dt[target.x, tbincid]
  
  # Define age target
  age.limit.older.than <- target.dt[target.x, age.low]
  age.limit.younger.than <- target.dt[target.x, age.high]
  
  # Function that defines target population for model run
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
  
  # Run the data prep
  source("CB-TLTBI_DataPreparation.R")
  
  # Run the model
  source("CB-TLTBI.R")
  
  # Run the analysis file to sort the 
  # model output and put the main findings 
  # into a table called "table1"
  source("CEA analysis.R")
  
  # Add some intial columns in table1
  # that specifies the target group
  table1 <- data.table(rep(target.tbincid, nrow(table1)), table1)
  table1 <- data.table(rep(age.limit.younger.than - 1, nrow(table1)), table1)
  table1 <- data.table(rep(age.limit.older.than + 1, nrow(table1)), table1)
  setnames(table1, 1, "age.low")
  setnames(table1, 2, "age.high")
  setnames(table1, 3, "tbincid")
  
  # Bind the results from each model run together
  # into one large table
  if (target.x == 1) {
    results.dt <- copy(table1)
  } else {
    results.dt <- rbind(results.dt, table1)
  }
  
  # remove the files in the output folder
  file.remove(filenames)

}

# Save the output to file
if (onshore == 1) {
  saveRDS(results.dt, file = "Data/onshore_results.rds")
} else if (onshore == 0) {
  saveRDS(results.dt, file = "Data/offshore_results.rds")
}

check <- unique( results.dt )

check <- results.dt[!duplicated(results.dt)]


# Write the table to clipboard so I can paste it into Excel
write.table(results.dt, file = "clipboard-16384", 
            sep = "\t", row.names = FALSE)  

check <- readRDS("Data/offshore_results.rds")
