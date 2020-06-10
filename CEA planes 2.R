
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
############################  ##########################################################################


# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
params <- readRDS("params onshore.rds")
onshore <- 1


#  params <- readRDS("params offshore.rds")
# onshore <- 0
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

# Create a datatable that contains all of the combinations
# of targets, i.e. by age and TB incidence in country of birth

# Define the three targets I want to present

if (onshore == 1) {
  
  tbincid <- c(rep("200+", 7))
  age.low <- c(rep(10, 7)) 
  age.high <- c(rep(66, 7))
  
} else if (onshore == 0) {
  
  tbincid <- c(rep("100+", 7))
  age.low <- c(rep(10, 7)) 
  age.high <- c(rep(36, 7))
  
}

######################################################################################################
other <- c("lifetime horizon",
           "All specialist",
           "LTBI decrement",
           "Perfect cascade",
           "30yr horizon", 
           "30 inflows and horizon",
           "perm emigration")
target.dt <- data.table(age.low, age.high, tbincid, other)


# The following loops down the rows of the table
# and runs the model with each specified target
# Then the output is analysed and entered
# into a new enormous data table.

# Testing:
# target.x <- 2
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
  
  if (other.cat == "lifetime horizon") {
    
    
    
  } else if (other.cat == "30yr horizon") {
    
    totalcycles <- 30
    finalyear <- startyear + totalcycles
    final.year <- finalyear
    
  } else if (other.cat == "All specialist") {
    
    prop.spec <- 1
    
    if (onshore == 0) {
      
      cscreentst <- 0
      
      cscreenqft <- 0
      
      cattend <- c.spec.first + (c.mcs * chance.of.needing.mcs) + c.cxr
      
      c.spec.first <- c.spec.review
      
    } else if (onshore == 1) {
      
      cscreentst <- 244.70
      
      cscreenqft <- 210.57
      
      cattend <- c.spec.review + (c.mcs * chance.of.needing.mcs) + c.cxr
      
      c.spec.first <- c.spec.review
      
    }
    
    # 3HP sort
    appt <- num.appt3HP * c.gp.review + c.liver
    spec.appt <- c.spec.first + (num.appt3HP - 1) * c.spec.review + c.liver
    ctreat3HP <- appt + cmed3HP
    cparttreat3HP <-  appt / part.appt + cmed3HP / part.med      
    ctreatspec3HP <-  spec.appt + cmed3HP 
    cparttreatspec3HP <-  spec.appt / part.appt + cmed3HP / part.med
    # 4r sort
    appt <- num.appt4R * c.gp.review
    spec.appt <- c.spec.first + (num.appt4R - 1) * c.spec.review
    ctreat4R <- appt + cmed4R
    cparttreat4R <-  appt / part.appt + cmed4R / part.med      
    ctreatspec4R <-  spec.appt + cmed4R 
    cparttreatspec4R <-  spec.appt / part.appt + cmed4R / part.med
    # 6H sort
    appt <- num.appt6H * c.gp.review + c.liver
    spec.appt <- c.spec.first + (num.appt6H - 1) * c.spec.review + c.liver
    ctreat6H <- appt + cmed6H
    cparttreat6H <-  appt / part.appt + cmed6H / part.med      
    ctreatspec6H <-  spec.appt + cmed6H 
    cparttreatspec6H <-  spec.appt / part.appt + cmed6H / part.med
    # 9H sort
    appt <- num.appt9H * c.gp.review + c.liver
    spec.appt <- c.spec.first + (num.appt9H - 1) * c.spec.review + c.liver
    ctreat9H <- appt + cmed9H
    cparttreat9H <-  appt / part.appt + cmed9H / part.med      
    ctreatspec9H <-  spec.appt + cmed9H 
    cparttreatspec9H <-  spec.appt / part.appt + cmed9H / part.med 
    
    
  } else if (other.cat == "perm emigration") {
    
    migrant.inflow.size <- 103740 # baseline 434340, permanent 103740
    
    totalcycles <- 98
    finalyear <- startyear + totalcycles
    final.year <- finalyear
    
    setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
    emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds")
    Get.EMIGRATE <- function(xDT, year) {
      
      DT <- copy(xDT[, .(year, AGEP, YARP)])
      
      # To lookup all ages beyond the killing off age
      DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
      
      # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
      emigrate.rate[Age > kill.off.above, upper := 0]
      
      emigrate.rate[DT[, .(AGEP)], upper, on = .(age = AGEP)] # use this one for a 14.76% perm rates
      
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
    
    } else if (other.cat == "30 inflows and horizon") {
      
      totalcycles <- 30
      finalinflow <- 29
      finalyear <- startyear + totalcycles
      final.year <- finalyear
      
    } else if (other.cat == "Perfect cascade") {
      
      # the perfect cascade
      att <- 1 
      attscreen <- 1
      begintrt <- 1
      treat.complete.3HP <- 1
      treat.complete.4R <- 1
      treat.complete.6H <- 1
      treat.complete.9H <- 1
      
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

# # Save the output to file
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
if (onshore == 1) {
  
  saveRDS(results.dt, file = "Data/cea.plane.2.onshore.rds")
  
} else if (onshore == 0) {
  
  saveRDS(results.dt, file = "Data/cea.plane.2.offshore.rds")
  
}



# Write the table to clipboard so I can paste it into Excel
write.table(results.dt, file = "clipboard-16384", sep = "\t", row.names = FALSE)
