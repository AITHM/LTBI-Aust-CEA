
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

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("CB-TLTBI Functions.R")
source("Parameter values.R")
  
totalcycles <- 30
  
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# Run the data prep
source("CB-TLTBI_DataPreparation.R")
  
parameters.already.set <- 1
  
# Run the model
source("CB-TLTBI.R")
  
# Read in the output files
filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds", full.names = TRUE)
files <- lapply(filenames, readRDS)
  
# Create a list of the names of the output files
namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds")
namelist <- gsub("\\b.rds\\b", "", namelist)
namelist <- gsub("\\bS2\\b", "", namelist)
namelist <- substring(namelist, 2)
  
# Name the files in the list
files <- setNames(files, namelist)
  
# Create a column with the YEAR and strategy name within each data table
counter <- 0
files <- lapply(files, function(dt) {
    dt <- as.data.table(dt)
    dt[, YEAR :=  cycle + start.year]
    counter <<- counter + 1
    dt[, STRAT := namelist[counter]]
})
  
# Finding the baseline quantities
base.all <- files[[1]]



# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("CB-TLTBI Functions.R")
source("Parameter values.R")

migrant.inflow.size <- 103740 # baseline 434340, permanent 103740

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds")
Get.EMIGRATE <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGERP, YARP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  emigrate.rate[DT[, .(AGERP)], upper, on = .(Age = AGERP)] # use this one for a 14.76% perm rates
}

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# Run the data prep
source("CB-TLTBI_DataPreparation.R")

parameters.already.set <- 1

# Run the model
source("CB-TLTBI.R")

# Read in the output files
filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds", full.names = TRUE)
files <- lapply(filenames, readRDS)

# Create a list of the names of the output files
namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds")
namelist <- gsub("\\b.rds\\b", "", namelist)
namelist <- gsub("\\bS2\\b", "", namelist)
namelist <- substring(namelist, 2)

# Name the files in the list
files <- setNames(files, namelist)

# Create a column with the YEAR and strategy name within each data table
counter <- 0
files <- lapply(files, function(dt) {
  dt <- as.data.table(dt)
  dt[, YEAR :=  cycle + start.year]
  counter <<- counter + 1
  dt[, STRAT := namelist[counter]]
})

# Finding the baseline quantities
dt.perm <- files[[2]]

# Another run for all temp migrants, as if they are not screened, i.e. their base cost
# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("CB-TLTBI Functions.R")
source("Parameter values.R")

migrant.inflow.size <- 434340 - 103740 # baseline 434340, permanent 103740

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# Run the data prep
source("CB-TLTBI_DataPreparation.R")

parameters.already.set <- 1

# Run the model
source("CB-TLTBI.R")

# Read in the output files
filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds", full.names = TRUE)
files <- lapply(filenames, readRDS)

# Create a list of the names of the output files
namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds")
namelist <- gsub("\\b.rds\\b", "", namelist)
namelist <- gsub("\\bS2\\b", "", namelist)
namelist <- substring(namelist, 2)

# Name the files in the list
files <- setNames(files, namelist)

# Create a column with the YEAR and strategy name within each data table
counter <- 0
files <- lapply(files, function(dt) {
  dt <- as.data.table(dt)
  dt[, YEAR :=  cycle + start.year]
  counter <<- counter + 1
  dt[, STRAT := namelist[counter]]
})

# Finding the baseline quantities
dt.temp <- files[[1]]


# Sorting the base row:
base <- copy(base.all)

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
  dt <- as.data.table(dt)
  
  # Add the name of the strategy
  nameofdt <- dt$STRAT[1]
  
  # annual migrant inflow
  migflow <- dt[YEAR == start.year & YARP == start.year, sum(NUMP),]
  
  # percentage with LTBI
  cdt <- targetfunc(dt)
  cdt <- as.data.table(cdt)
  percentltbi <- (cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]/cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * 100
  
  # annual average number emigrating
  emigflow <- dt[YEAR == final.year, sum(p.emigrate)]/totalcycles
  
  # probability of emigration over time horizon
  emigpercent <- (dt[YEAR == final.year, sum(p.emigrate)] / migflow) * 100
  
  # one year probability of emigration 
  emigproboneyear <- (-1/totalcycles * log(1 - (emigpercent/100))) * 100
  
  # annual number screened/tested
  # all those in every state except the "no test" states (and ignoring TB cases)
  numscreened <- dt[YEAR == start.year + 1 & YARP == start.year,
                    sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
                      sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
                      sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numscreened <- (cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * attscreen
  
  # annual number referred 
  # this will either be:
  # those with and without LTBI in the target population (which I get using the targetfunc) 
  # multiplied by the relevent sensitivities and specificities and multiplied by
  # the proportion that attended the screen:
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numref <- ((cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * sntst10) +
  #   ((cdt[YEAR == start.year & YARP == start.year, sum(NUMP),] -
  #      cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * (1 - sptst10))
  
  # or it will be the number that attended in each of the LTBI and SUS categories
  # divided by the chance of attending
  numref <- dt[YEAR == start.year + 1 & YARP == start.year, sum(p.sus.nbt) +
                 sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] / att
  
  
  
  # number attending annually
  # all those in the "did not begin treatment", "did not complete treatment" etc.
  numatt <- dt[YEAR == start.year + 1 & YARP == start.year, 
               sum(p.sus.nbt) + sum(p.sus.nct) +
                 sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number screened/tested
  totscreen <- numscreened
  
  # total number attended during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  totatt <- cdt[, sum(p.sus.nbt) + sum(p.sus.nct) +
                  sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                  sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # total number treated (any treatment) during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  numberstarttreat <- cdt[, sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) +
                            sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number treated (effective) during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  cdt[YEAR == YARP + 1, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  numbertreated <- cdt[, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  
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
  
  # total number of tb cases
  tbtotal <- dt[, sum(p.tb)]
  
  # number of tb cases prevented
  tbprev <- basetbcount - tbtotal
  
  # percentage of tb cases prevented
  tbprevpercent <- (tbprev/basetbcount) * 100
  
  # number of tb deaths 
  tbdeath <- dt[YEAR == final.year, sum(p.tb.death)]
  
  # number of tb deaths prevented
  tbdeathprev <- basetbdeath - tbdeath
  
  #Cost per TB death prevented
  costpertbdeath <- totaddcost/tbdeathprev
  
  #Cost per TB case prevented
  costpertb <- totaddcost/tbprev
  
  # number needed to screen (to prevent a tb case)
  nns <- totscreen/tbprev
  
  # number needed to effectively treat (to prevent a tb case)
  nnt <- numbertreated/tbprev
  
  # number needed to at least start treat (to prevent a tb case)
  nnbt <- numberstarttreat/tbprev
  
  
  
  
  # number of SAEs among those with ltbi
  saeltbi <- dt[YEAR == YARP + 1, sum(p.ltbi.sae)]
  
  # number of SAEs among those without ltbi
  saesus <- dt[YEAR == YARP + 1, sum(p.sus.sae)]
  
  # number of SAE deaths among those with ltbi
  saedeathltbi <- dt[YEAR == YARP + 2, sum(p.ltbi.sae.death)]
  
  # number of SAE deaths among those without ltbi
  saedeathusus <- dt[YEAR == YARP + 2, sum(p.sus.sae.death)]
  
  # total baseline QALYS
  qalybase
  
  # total number of QALYS
  a <- which( colnames(dt) == "SQ.p.sus" )
  b <- which( colnames(dt) == "SQ.p.emigrate" )
  dt$SQsum <- rowSums(dt[, a:b], na.rm =TRUE)
  qalytot <- sum(dt$SQsum)
  
  #Incremental QALYs per 1000
  incremqaly1000 <- ((qalytot - qalybase)/totscreen)*1000
  
  #Incremental QALYs
  incremqaly <- qalytot - qalybase
  
  # Cost per QALY - ICER
  costperqaly <- totaddcost/incremqaly
  
  tablist<-list(nameofdt,
                migflow,
                percentltbi,
                emigflow,
                emigpercent,
                emigproboneyear,
                numscreened,
                numref,
                numatt,
                totscreen,
                totatt,
                numberstarttreat,
                numbertreated,
                totbasecost,
                totcost,
                totaddcost,
                tbtotal,
                tbprev,
                tbprevpercent,
                tbdeath,
                tbdeathprev,
                costpertbdeath,
                costpertb,
                nns,
                nnt,
                nnbt,
                saeltbi,
                saesus,
                saedeathltbi,
                saedeathusus,
                qalybase,
                qalytot,
                incremqaly1000,
                incremqaly,
                costperqaly)
  
  tablist[is.na(tablist)] <- 0
  
  namelist<-c("strategy",
              "annual migrant flow",
              "percent of cohort with ltbi",
              "annual average number emigrating",
              "Probability of emigration over time horizon",
              "Probability of emigration in one year",
              "annual number screened",
              "annual number referred",
              "annual number that attended",
              "total number screened",
              "total number attended",
              "total number beginning treatment",
              "total number treated effectively",
              "total base cost",
              "total cost",
              "total additional cost",
              "total TB cases",
              "TB cases prevented",
              "Percentage of all TB cases prevented",
              "total TB deaths",
              "total TB deaths prevented",
              "cost per TB death prevented",
              "cost per TB case prevented",
              "Number needed to screen",
              "Number needed to treat",
              "Number needed to begin treatment",
              "number of SAEs in those with ltbi",
              "number of SAEs in those without ltbi",
              "number of SAE deaths among those with ltbi",
              "number of SAE deaths among those without ltbi",
              "Baseline QALY total",
              "Strategy QALYS total",
              "Incremental QALYS per 1000",
              "Incremental QALYS",
              "ICER, i.e. Cost per QALY")
  names(tablist) <- namelist
  pop <- as.data.frame(tablist)
  pop
}

base.row <- tabfunc(base)






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

 
  dt.perm <- as.data.table(dt.perm)
  dt.temp <- as.data.table(dt.temp)
  
  # Add the name of the strategy
  nameofdt <- dt.perm$STRAT[1]
  
  # annual migrant inflow
  migflow <- dt.perm[YEAR == start.year & YARP == start.year, sum(NUMP),] +
    dt.temp[YEAR == start.year & YARP == start.year, sum(NUMP),]
  
  # percentage with LTBI
  cdt <- targetfunc(base)
  cdt <- as.data.table(cdt)
  percentltbi <- (cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]/cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * 100
  
  # annual average number emigrating
  emigflow <- base[YEAR == final.year, sum(p.emigrate)]/totalcycles
  
  # probability of emigration over time horizon
  emigpercent <- (base[YEAR == final.year, sum(p.emigrate)] / migflow) * 100
  
  # one year probability of emigration 
  emigproboneyear <- (-1/totalcycles * log(1 - (emigpercent/100))) * 100
  
  # annual number screened/tested
  # all those in every state except the "no test" states (and ignoring TB cases)
  numscreened <- dt.perm[YEAR == start.year + 1 & YARP == start.year,
                    sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
                      sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
                      sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numscreened <- (cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * attscreen
  
  # annual number referred 
  # this will either be:
  # those with and without LTBI in the target population (which I get using the targetfunc) 
  # multiplied by the relevent sensitivities and specificities and multiplied by
  # the proportion that attended the screen:
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numref <- ((cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * sntst10) +
  #   ((cdt[YEAR == start.year & YARP == start.year, sum(NUMP),] -
  #      cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * (1 - sptst10))
  
  # or it will be the number that attended in each of the LTBI and SUS categories
  # divided by the chance of attending
  numref <- dt.perm[YEAR == start.year + 1 & YARP == start.year, sum(p.sus.nbt) +
                 sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] / att
  
  
  
  # number attending annually
  # all those in the "did not begin treatment", "did not complete treatment" etc.
  numatt <- dt.perm[YEAR == start.year + 1 & YARP == start.year, 
               sum(p.sus.nbt) + sum(p.sus.nct) +
                 sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number screened/tested
  totscreen <- numscreened
  
  # total number attended during the whole time period
  cdt <- copy(dt.perm)
  cdt <- as.data.table(cdt)
  totatt <- cdt[, sum(p.sus.nbt) + sum(p.sus.nct) +
                  sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                  sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # total number treated (any treatment) during the whole time period
  cdt <- copy(dt.perm)
  cdt <- as.data.table(cdt)
  numberstarttreat <- cdt[, sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) +
                            sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number treated (effective) during the whole time period
  cdt <- copy(dt.perm)
  cdt <- as.data.table(cdt)
  cdt[YEAR == YARP + 1, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  numbertreated <- cdt[, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  
  # total base cost
  totbasecost
  
  # total cost of strategy
  a <- which( colnames(dt.perm) == "SC.p.sus" )
  b <- which( colnames(dt.perm) == "SC.p.emigrate" )
  dt.perm$SCsum.perm <- rowSums(dt.perm[, a:b], na.rm = TRUE)
  
  a <- which( colnames(dt.temp) == "SC.p.sus" )
  b <- which( colnames(dt.temp) == "SC.p.emigrate" )
  dt.temp$SCsum.temp <- rowSums(dt.temp[, a:b], na.rm = TRUE)
  
  a <- which( colnames(dt.perm) == "FC.p.sus" )
  b <- which( colnames(dt.perm) == "FC.p.emigrate" )
  dt.perm$FCsum.perm <- rowSums(dt.perm[, a:b], na.rm = TRUE)
  
  a <- which( colnames(dt.temp) == "FC.p.sus" )
  b <- which( colnames(dt.temp) == "FC.p.emigrate" )
  dt.temp$FCsum.temp <- rowSums(dt.temp[, a:b], na.rm = TRUE)
 
  totcost <- sum(dt.perm$SCsum.perm) + sum(dt.temp$SCsum.temp) +
    sum(dt.perm$FCsum.perm) + sum(dt.temp$FCsum.temp)
  
  # incremental cost of strategy
  totaddcost <- totcost - totbasecost
  
  # total number of tb cases
  tbtotal <- dt.perm[, sum(p.tb)] + dt.temp[, sum(p.tb)] 
  
  # number of tb cases prevented
  tbprev <- basetbcount - tbtotal
  
  # percentage of tb cases prevented
  tbprevpercent <- (tbprev/basetbcount) * 100
  
  # number of tb deaths 
  tbdeath <- dt.perm[YEAR == final.year, sum(p.tb.death)] +
    dt.temp[YEAR == final.year, sum(p.tb.death)]
  
  # number of tb deaths prevented
  tbdeathprev <- basetbdeath - tbdeath
  
  #Cost per TB death prevented
  costpertbdeath <- totaddcost/tbdeathprev
  
  #Cost per TB case prevented
  costpertb <- totaddcost/tbprev
  
  # number needed to screen (to prevent a tb case)
  nns <- totscreen/tbprev
  
  # number needed to effectively treat (to prevent a tb case)
  nnt <- numbertreated/tbprev
  
  # number needed to at least start treat (to prevent a tb case)
  nnbt <- numberstarttreat/tbprev
  
  # number of SAEs among those with ltbi
  saeltbi <- dt.perm[YEAR == YARP + 1, sum(p.ltbi.sae)] +
    dt.temp[YEAR == YARP + 1, sum(p.ltbi.sae)]
  
  # number of SAEs among those without ltbi
  saesus <- dt.perm[YEAR == YARP + 1, sum(p.sus.sae)] +
    dt.temp[YEAR == YARP + 1, sum(p.sus.sae)]
  
  # number of SAE deaths among those with ltbi
  saedeathltbi <- dt.perm[YEAR == YARP + 2, sum(p.ltbi.sae.death)] +
    dt.temp[YEAR == YARP + 2, sum(p.ltbi.sae.death)]
  
  # number of SAE deaths among those without ltbi
  saedeathusus <- dt.perm[YEAR == YARP + 2, sum(p.sus.sae.death)] +
    dt.temp[YEAR == YARP + 2, sum(p.sus.sae.death)]
  
  # total baseline QALYS
  qalybase
  
  # total number of QALYS
  a <- which( colnames(dt.perm) == "SQ.p.sus" )
  b <- which( colnames(dt.perm) == "SQ.p.emigrate" )
  dt.perm$SQsum <- rowSums(dt.perm[, a:b], na.rm = TRUE)
  
  a <- which( colnames(dt.temp) == "SQ.p.sus" )
  b <- which( colnames(dt.temp) == "SQ.p.emigrate" )
  dt.temp$SQsum <- rowSums(dt.temp[, a:b], na.rm = TRUE)
  
  qalytot <- sum(dt.perm$SQsum) + sum(dt.temp$SQsum)
  
  #Incremental QALYs per 1000
  incremqaly1000 <- ((qalytot - qalybase)/totscreen)*1000
  
  #Incremental QALYs
  incremqaly <- qalytot - qalybase
  
  # Cost per QALY - ICER
  costperqaly <- totaddcost/incremqaly
  
  tablist<-list(nameofdt,
                migflow,
                percentltbi,
                emigflow,
                emigpercent,
                emigproboneyear,
                numscreened,
                numref,
                numatt,
                totscreen,
                totatt,
                numberstarttreat,
                numbertreated,
                totbasecost,
                totcost,
                totaddcost,
                tbtotal,
                tbprev,
                tbprevpercent,
                tbdeath,
                tbdeathprev,
                costpertbdeath,
                costpertb,
                nns,
                nnt,
                nnbt,
                saeltbi,
                saesus,
                saedeathltbi,
                saedeathusus,
                qalybase,
                qalytot,
                incremqaly1000,
                incremqaly,
                costperqaly)
  
  tablist[is.na(tablist)] <- 0
  
  namelist<-c("strategy",
              "annual migrant flow",
              "percent of cohort with ltbi",
              "annual average number emigrating",
              "Probability of emigration over time horizon",
              "Probability of emigration in one year",
              "annual number screened",
              "annual number referred",
              "annual number that attended",
              "total number screened",
              "total number attended",
              "total number beginning treatment",
              "total number treated effectively",
              "total base cost",
              "total cost",
              "total additional cost",
              "total TB cases",
              "TB cases prevented",
              "Percentage of all TB cases prevented",
              "total TB deaths",
              "total TB deaths prevented",
              "cost per TB death prevented",
              "cost per TB case prevented",
              "Number needed to screen",
              "Number needed to treat",
              "Number needed to begin treatment",
              "number of SAEs in those with ltbi",
              "number of SAEs in those without ltbi",
              "number of SAE deaths among those with ltbi",
              "number of SAE deaths among those without ltbi",
              "Baseline QALY total",
              "Strategy QALYS total",
              "Incremental QALYS per 1000",
              "Incremental QALYS",
              "ICER, i.e. Cost per QALY")
  names(tablist) <- namelist
  strat.row <- as.data.frame(tablist)

final <- rbind(base.row, strat.row)

# Write the table to clipboard so I can paste it into Excel
write.table(final, "clipboard", sep = "\t", row.names = FALSE)


