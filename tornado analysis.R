
# This script creates a new table called 
# tornado.dt. that has a list of all the parameters and
# limits that we'd like to examine in the tornado plot.
# Then it loops down the rows of the table
# and runs the model with the specified parameter limit
# in each row.
# The output is then analysed and the icer is entered
# into the tornado.dt data table.

######################################################################################################
################### ###################################################################################
# MAKE SURE THE DATA OUTPUTS FOLDER IS EMPTY BEFORE RUNNING THIS
######################################################################################################
######################################################################################################

library(data.table)
library(reshape2) 

parameters.already.set <- 1

# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

sensfunc <- function(paramname, loworhigh) { 
  paramname <- deparse(substitute(paramname))
  colname <- deparse(substitute(loworhigh))
  newvalue <- params[p == paramname, ..colname]
  params[p == paramname, mid:= newvalue]
}

# Create a datatable that will contain the
# parameter names and limits and,
# eventually, the results 
tornado.dt <- copy(params)
tornado.dt <- as.data.table(tornado.dt)
tornado.dt[, distribution := NULL]
tornado.dt[, shape := NULL]
tornado.dt[, mid := NULL]
tornado.dt[, icer := NA]
tornado.dt[, icer := as.numeric(icer)]
pselect<- c("sntst15","sptst15",
            "sntst10","sptst10",
            "treatr4R","cmed4R",
            "cparttreat4R","ttt4R",
            "ctb","att",
            "begintrt","saemr",
            "uactivetbr","uactivetb",
            "ultbi4R", "csae", "num.appt4R",
            "prop.spec", "ultbitreatsae", "rradj")
tornado.dt <- tornado.dt[tornado.dt$p %in% pselect,]

tornado.dt <- melt(tornado.dt, id = c("p", "icer"))
tornado.dt <- subset(tornado.dt, !is.na(value))
# Adding some other variables
tornado.dt <- rbindlist(list(tornado.dt,  data.table(p = "disc", 
                                                     icer = NA, 
                                                     variable = "low", 
                                                     value = 0.00)))
tornado.dt <- rbindlist(list(tornado.dt,  data.table(p = "disc", 
                                                     icer = NA, 
                                                     variable = "high", 
                                                     value = 0.05)))
tornado.dt <- rbindlist(list(tornado.dt,  data.table(p = "totalcycles", 
                                                     icer = NA, 
                                                     variable = "low", 
                                                     value = 10)))
tornado.dt <- rbindlist(list(tornado.dt,  data.table(p = "totalcycles", 
                                                     icer = NA, 
                                                     variable = "high", 
                                                     value = 80)))

# tornado.dt <- tornado.dt[6:8, ]


# The following loops down the rows of the table
# and runs the model with each specified parameter limit.
# Then the output is analysed and the icer is entered
# into the tornado.dt data table.
for(tornado.x in 1:nrow(tornado.dt)) {
  
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  source("CB-TLTBI Functions.R")
  source("Parameter values.R")
  paraname <- tornado.dt[tornado.x, p]
  limit <- tornado.dt[tornado.x, value]
  assign(paraname, limit)
  # Adjusting the partial LTBI treatment utilities so 
  # they are dependent on the value of
  # the sampled utility for full treatment
  part.utility.dec <- 0.5
  ultbipart3HP <- uhealthy - ((uhealthy - ultbi3HP) * part.utility.dec)
  ultbipart4R <- uhealthy - ((uhealthy - ultbi4R) * part.utility.dec)
  ultbipart6H <- uhealthy - ((uhealthy - ultbi6H) * part.utility.dec)
  ultbipart9H <- uhealthy - ((uhealthy - ultbi9H) * part.utility.dec)
  
  # Sourcing the medical costs
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  source("Medical costs.R")
  
  # These specify how much of the appointment and medicine
  # costs are applied for the partial costs and treatment
  part.appt <- 2
  part.med <- 3
  
  c.gp.first <- c.gp.c.vr * (1 - proportion.nonvr) + c.gp.c.nonvr * proportion.nonvr
  
  c.gp.review <- c.gp.b.vr * (1 - proportion.nonvr) + c.gp.b.nonvr * proportion.nonvr
  
  chance.of.needing.mcs <- 0.1
  
  # Cost of initial appointment after positive screen
  # These costs are different for on and off-shore screening
  # so this need to be taken into account, i.e. for onshore
  # screening this appointment will be a review with the GP
  # or it may be the first appointment with a specialist
  # and a liver function test will be ordered
  # Also, all ongoing appointments related to LTBI treatment will be
  # review appointments
  
  if (onshore == 0) {
    cattend <- c.gp.first + (c.mcs * chance.of.needing.mcs) + c.cxr
  } else if (onshore == 1) {
    cattend <- ((c.gp.review + (c.mcs * chance.of.needing.mcs) +
                   + c.cxr) * (1 - prop.spec)) + 
      ((c.spec.first + (c.mcs * chance.of.needing.mcs) +
          + c.cxr) * prop.spec)
  }
  
  if (onshore == 1) {
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

  # Below runs the data prep
  source("CB-TLTBI_DataPreparation.R")
  # Below runs the model
  tornado.analysis <- 1
  source("CB-TLTBI.R")
  
  # This is the modified script form the cea analysis R file that analyses the 
  # output data from the cost-effectiveness analysis.
  library(tidyverse)
  library(data.table)

  # Read in the output files
  filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", 
                          pattern = "*.rds", full.names = TRUE)
  files <- lapply(filenames, readRDS)
  
  # Create a list of the names of the output files
  namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", 
                         pattern = "*.rds")
  namelist <- gsub("\\b.rds\\b", "", namelist)
  namelist <- gsub("\\bS2\\b", "", namelist)
  namelist <- substring(namelist, 2)
  
  # Create a column with the YEAR and strategy name within each data table
  counter <- 0
  files <- lapply(files, function(dt) {
    dt <- as.data.table(dt)
    dt[, YEAR :=  cycle + start.year]
    counter <<- counter + 1
    dt[, STRAT := namelist[counter]]
  })
  
  # Name the files in the list
  files <- setNames(files, namelist)
  
  # Finding the baseline quantities
  base <- files[[1]]
  dt <- files[[2]]
  
  # total baseline cost
  a <- which( colnames(base) == "SC.p.sus" )
  b <- which( colnames(base) == "SC.p.emigrate" )
  base$SCsum <- rowSums(base[, a:b], na.rm = TRUE)
  a <- which( colnames(base) == "FC.p.sus" )
  b <- which( colnames(base) == "FC.p.emigrate" )
  base$FCsum <- rowSums(base[, a:b], na.rm = TRUE)
  totbasecost <- sum(base$SCsum) + sum(base$FCsum)
  
  # total baseline QALYS
  a <- which( colnames(base) == "SQ.p.sus" )
  b <- which( colnames(base) == "SQ.p.emigrate" )
  base$SQsum <- rowSums(base[, a:b], na.rm = TRUE)
  qalybase <- sum(base$SQsum)
  
  # total baseline tb cases
  basetbcount <- base[, sum(p.tb)]
  
  # total baseline tb cases
  basetbdeath <- base[YEAR == final.year, sum(p.tb.death)]
  
  tabfunc <- function(dt) { 
    
    # total base cost
    totbasecost
    
    # total cost of strategy
    a <- which( colnames(dt) == "SC.p.sus" )
    b <- which( colnames(dt) == "SC.p.emigrate" )
    dt$SCsum <- rowSums(dt[, a:b], na.rm = TRUE)
    a <- which( colnames(dt) == "FC.p.sus" )
    b <- which( colnames(dt) == "FC.p.emigrate" )
    dt$FCsum <- rowSums(dt[, a:b], na.rm = TRUE)
    sum(dt$FCsum)
    totcost <- sum(dt$SCsum) + sum(dt$FCsum)
    
    # incremental cost of strategy
    totaddcost <- totcost - totbasecost
    
    # total baseline QALYS
    qalybase
    
    # total number of QALYS
    a <- which( colnames(dt) == "SQ.p.sus" )
    b <- which( colnames(dt) == "SQ.p.emigrate" )
    dt$SQsum <- rowSums(dt[, a:b], na.rm =TRUE)
    qalytot <- sum(dt$SQsum)
    
    #Incremental QALYs
    incremqaly <- qalytot - qalybase
    
    # Cost per QALY - ICER
    costperqaly <- totaddcost/incremqaly
    
    tablist<-list(costperqaly)
    
    tablist[is.na(tablist)] <- 0
    
    namelist<-c("ICER, i.e. Cost per QALY")
    names(tablist) <- namelist
    pop <- as.data.frame(tablist)
    pop
  }
  
  # Create the table
  check <- unlist(lapply(files, tabfunc))

  tornado.dt[tornado.x, icer := as.numeric(check[[2]])]

}



# Write the table to clipboard so I can paste it into Excel
write.table(tornado.dt, "clipboard", sep = "\t", row.names = FALSE)









