
# This script runs several analyses, one after the other
# with different target groups and time horizons
# and puts the results into a large table that is saved. 
# This table can then be used to create the CEA planes.

library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)

# # Reading in the data
# setwd("H:/Katie/PhD/CEA/Data")
# df <- read.csv("ltbi utility plot.csv")

parameters.already.set <- 1


######################################################################################################
################### ###################################################################################
# MAKE SURE THE DATA OUTPUTS FOLDER IS EMPTY BEFORE RUNNING THIS
######################################################################################################
######################################################################################################


# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

# Create a datatable that contains all of the combinations
# of targets, i.e. by age and TB incidence in country of birth

# Define the three targets I want to present
age.low <- c(rep(10, 5)) 
age.high <- c(rep(66, 5))
tbincid <- c(rep("100+",5))
other <- c("30yr horizon", "80yr horizon", 
           "perm emigration", "LTBI decrement",
           "30 inflows")
target.dt <- data.table(age.low, age.high, tbincid, other)


# The following loops down the rows of the table
# and runs the model with each specified target
# Then the output is analysed and entered
# into a new enormous data table.

# Testing:
# target.x <- 3
# target.dt <- target.dt[c(1,3),]

for(target.x in 1:nrow(target.dt)) {
  
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  source("CB-TLTBI Functions.R")
  source("Parameter values.R")
  
  # Define target: TB incidence in country of birth: "40+" "100+" "150+" "200+"
  target.tbincid <- target.dt[target.x, tbincid]
  
  # Define age target
  age.limit.older.than <- target.dt[target.x, age.low]
  age.limit.younger.than <- target.dt[target.x, age.high]
  other.cat <- target.dt[target.x, other]
  
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
  
  if (other.cat == "30yr horizon") {
    
    totalcycles <- 30
    
  } else if (other.cat == "80yr horizon") {
    
    totalcycles <- 80
    
  } else if (other.cat == "perm emigration") {
    
    migrant.inflow.size <- 103740 # baseline 434340, permanent 103740
    
    setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
    emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds")
    Get.EMIGRATE <- function(xDT, year) {
      
      DT <- copy(xDT[, .(year, AGERP, YARP)])
      
      DT[AGERP > 110, AGERP := 110]
      
      emigrate.rate[DT[, .(AGERP)], upper, on = .(Age = AGERP)] # use this one for a 14.76% perm rates
    }
    
    } else if (other.cat == "LTBI decrement") {
    
    ultbi3HP <- params[p == "ultbi3HP", low]
    ultbi4R <- params[p == "ultbi4R", low]
    ultbi6H <- params[p == "ultbi6H", low]
    ultbi9H <- params[p == "ultbi9H", low]
    
    # Adjusting the partial LTBI treatment utilities so 
    # they are dependent on the value of
    # the sampled utility for full treatment
    part.utility.dec <- 0.5
    ultbipart3HP <- uhealthy - ((uhealthy - ultbi3HP) * part.utility.dec)
    ultbipart4R <- uhealthy - ((uhealthy - ultbi4R) * part.utility.dec)
    ultbipart6H <- uhealthy - ((uhealthy - ultbi6H) * part.utility.dec)
    ultbipart9H <- uhealthy - ((uhealthy - ultbi9H) * part.utility.dec)
    
    } else if (other.cat == "30 inflows") {
      finalinflow <- 29
    }
  
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI") 
  # Run the data prep
  source("CB-TLTBI_DataPreparation.R")
  
  parameters.already.set <- 1
  
  # Run the model
  source("CB-TLTBI.R")
  
  # Run the analysis file to sort the 
  # model output and put t he main findings 
  # into a table called "table1"
  source("CEA analysis.R")
  
  # Add some intial columns in table1
  # that specifies the target group
  table1 <- data.table(rep(other.cat, nrow(table1)), table1)
  setnames(table1, 1, "other")
  
  # Bind the results from each model run together
  # into one large table
  if (target.x == 1) {
    results.dt <- copy(table1)
  } else {
    results.dt <- rbind(results.dt, table1)
  }
  
}

# Save the output to file
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
saveRDS(results.dt, file = "Data/cea.plane.2.rds")

# Write the table to clipboard so I can paste it into Excel
write.table(results.dt, file = "clipboard-16384", sep = "\t", row.names = FALSE)
