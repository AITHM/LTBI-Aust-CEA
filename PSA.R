
# This is the code for the probabilistic sensitivity analysis (PSA)

# The model setup and all parameter values are defined below
# The "CB-TLTBI.R" and "Parameter values" scripts are not needed for PSA...
# ...the model setup and parameter values are all located within this file.

# This script should run the model Num_SIm times
# and output the incremental qalys and incremental costs of each run
# so that the cost-effectiveness plane and acceptability curves can be plotted.

# Coding style
# https://google.github.io/styleguide/Rguide.xml

options(scipen = 999)
library(data.table)
library(purrr)
library(ggplot2)
library(scales)
library(lazyeval) 
library(data.table) 

# Sourcing required functions from other scripts
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("CB-TLTBI Functions.R") # contains many functions necessary to run the model
source("CB-TLTBI_DataPreparation.R") # for sorting the population data
source("Distribution parameter calculations.R") # for determining distribution parameter values

################## PSA #####################################

# Defining the number of simulations we want
Num_SIm <- 10000

# Create a datatable that will eventually contain the
# parameter values to be used for each simulation/model run.
simrun <- c(seq(1, Num_SIm))
simdata <- as.data.frame(simrun)
simdata <- as.data.table(simdata)

# Define all the parameters that are don't have any uncertainty:
discount <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
start.year <- 2020 # start.year
totalcycles <- 10  # cycles ... The mortality data continues until 2100 and migrant 
cycles <- totalcycles 
# inflows are possible until 2050
final.year <- start.year + totalcycles

# The tests and treatments I want to consider in this analysis
testlist <- c("QTFGIT") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("3HP") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# The number of migrant inflows I want to include
# the migrant inflow will stop after the following Markov cycle
finalinflow <- 0

# Get.POP
# Target population
Get.POP <- function(DT, strategy) {
  
  # 200+
  # (ifelse(DT[, ISO3] == "200+", 1, 0)) & 
  # 150+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) & 
  # 100+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0)) &
  # 40+
  (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
    # Adjust age
    (ifelse(DT[, AGERP] > 10, 1, 0) &
       ifelse(DT[, AGERP] < 66, 1, 0))
  
}

# Define all the parameters that are uncertain, but that
# are also fixed for each model run (i.e. they aren't dependent
# on age or year etc).
# this data is in the PSA.csv and needs to be read in
setwd("H:/Katie/PhD/CEA")
dt <- read.csv("PSA.csv", header = TRUE)
dt <- as.data.table(dt)

dt[, X := NULL]
dt[, parameters := as.character(parameters)]
dt[, abbreviation := as.character(abbreviation)]
dt[, mid := as.numeric(as.character(mid))]
dt[, low := as.numeric(as.character(mid))]
dt[, high := as.numeric(as.character(mid))]
dt[, distribution := as.character(distribution)]

dt <- subset(dt, abbreviation == "snqftgit" |
                abbreviation == "spqftgit" |
                abbreviation == "snqftgit" |
                abbreviation == "sntst15" |
                abbreviation == "sntst10")

# The loop below adds one column to the simdata table based on the
# data in each row of the dt data frame
# i.e. it creates 10,000 samples for each variable to represent their uncertainty
for(i in 1:nrow(dt)) {
  abbreviation <- dt[i, abbreviation]
  mid <- dt[i, mid]
  low <- dt[i, low]
  high <- dt[i, high]
  distribution <- dt[i, distribution]
  if (distribution == "beta") {
    betaparam <- findbeta2(mid, low, high)
    simdata[, newcol := rbeta(Num_SIm, betaparam[1], betaparam[2])]
    setnames(simdata, "newcol", abbreviation)
  }
  else {
    betaparam <- findgamma2(mid, low, high)
    simdata[, newcol := rgamma(Num_SIm, betaparam[1], betaparam[2])]
    setnames(simdata, "newcol", abbreviation)
  }
}
  

# The dependent variables need to be defined separately...
# Get.RR
# Reactivation rates
Get.RR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGERP, SEXP, YARP, ISO3)])
  
  DT[ISO3 == "0-39" | ISO3 == "40-99", COBI := "<100"]  
  
  DT[ISO3 == "100-149" | ISO3 == "150-199" | ISO3 == "200+", COBI := "100+"]  
  
  DT[AGERP > 110, AGERP := 110]
  
  mid <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], Rate, on = .(aaa = AGERP, Sex = SEXP,
                                                                           ysa = ST, cobi = COBI)]
  low <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], LUI, on = .(aaa = AGERP, Sex = SEXP,
                                                                          ysa = ST, cobi = COBI)]
  high <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], UUI, on = .(aaa = AGERP, Sex = SEXP,
                                                                           ysa = ST, cobi = COBI)]
  betaparam <- findbeta2(mid, low, high)
  out <- rbeta(1, betaparam[1], betaparam[2])
  return(out)
  
} 
# Get.EMIGRATE
emigrate.rate <- readRDS("Data/emigrate.rate.rds") # BASELINE assumed rate incorporating both temp and permanent residents 
# emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds") # LOWER assumed rate among permanent residents
emigrate.rate <- as.data.table(emigrate.rate)

Get.EMIGRATE <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGERP, YARP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  mid <- emigrate.rate[DT[, .(AGERP)], 
                       Rate, on = .(Age = AGERP)]
  low <- emigrate.rate[DT[, .(AGERP)], 
                       lower, on = .(Age = AGERP)]
  high <- emigrate.rate[DT[, .(AGERP)], 
                        upper, on = .(Age = AGERP)]
  betaparam <- findbeta2(mid, low, high)
  rbeta(1, betaparam[1], betaparam[2])
}
# Get.MR ##### Do I need to incorporate uncertainty for this parameter?
# Look up the mortality rate from vic.mortality
Get.MR <- function(xDT, year, rate.assumption = "Med") {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond 110
  DT[AGEP > 100, AGEP := 100]
  
  out <- vic.mortality[Year == year & mrate == rate.assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]
  return(out)
  
}

# Get.TBMR
# Look up TB mortality rate
Get.TBMR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond 95 & 97
  DT[AGEP > 95 & SEXP == "Male", AGEP := 95]
  DT[AGEP > 97 & SEXP == "Female", AGEP := 97]
  DT[AGEP > 97 & SEXP == "Both", AGEP := 97]
  
  mid <- vic.tb.mortality[DT[, .(AGEP, SEXP)], 
                          Prob, on = .(age = AGEP, sex = SEXP)]
  low <- vic.tb.mortality[DT[, .(AGEP, SEXP)], 
                          lowerProb, on = .(age = AGEP, sex = SEXP)]
  high <- vic.tb.mortality[DT[, .(AGEP, SEXP)], 
                           upperProb, on = .(age = AGEP, sex = SEXP)]
  betaparam <- findbeta2(mid, low, high)
  out <- rbeta(1, betaparam[1], betaparam[2])
  return(out)
  
}
# Get.SAE
# Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAE <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  mid <- sae.rate[DT[, .(AGERP, treatment)], 
                  Rate, on = .(Age = AGERP, treatment = treatment)]
  low <- sae.rate[DT[, .(AGERP, treatment)], 
                  low, on = .(Age = AGERP, treatment = treatment)]
  high <- sae.rate[DT[, .(AGERP, treatment)], 
                   high, on = .(Age = AGERP, treatment = treatment)]
  betaparam <- findbeta2(mid, low, high)
  out <- rbeta(1, betaparam[1], betaparam[2])
  return(out)
  
}
# Get.SAEMR
# Look up the SAE mortality rate from sae.mortality (age and treatment dependent)
Get.SAEMR <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  mid <- sae.mortality[DT[, .(AGERP, treatment)], 
                       Rate, on = .(Age = AGERP, treatment = treatment)]
  low <- sae.mortality[DT[, .(AGERP, treatment)], 
                       low, on = .(Age = AGERP, treatment = treatment)]
  high <- sae.mortality[DT[, .(AGERP, treatment)], 
                        high, on = .(Age = AGERP, treatment = treatment)]
  betaparam <- findbeta2(mid, low, high)
  out <- rbeta(1, betaparam[1], betaparam[2])
  return(out)
  
}


# MODEL SET UP
# Reading in the data files required for the model run

vic.mortality <- readRDS("Data/aust.mortality.rds") # this is also required
vic.mortality <- as.data.table(vic.mortality)
# Projected mortality rates for Australia from:
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3222.02017%20(base)%20-%202066?OpenDocument
# Results then aggregated by gender assuming a gender weighting by age equivalent to the Australian population
# in September 2018. Source: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Sep%202018?OpenDocument
RRates <- readRDS("Data/RRatescobincidnosex.rds")
# TB reactivation rate data from: Dale K, Trauer J, et al. Estimating long-term tuberculosis 
# reactivation rates in Australian migrants. Clinical Infectious Diseases 2019 (in press)
vic.tb.mortality <- readRDS("Data/tb.mortality.rds")
vic.tb.mortality <- as.data.table(vic.tb.mortality)
vic.tb.mortality[, age := as.integer(age)]
# TB mortality data from: Dale K, Tay E, Trevan P, et al. Mortality among tuberculosis cases 
# in Victoria, 2002-2013: case fatality and factors associated with death. 
# Int J Tuberc Lung Dis 2016;20(4):515-23. doi: 10.5588/ijtld.15.0659
sae.rate <- readRDS("Data/sae.rate.rds") # this is also required
sae.rate <- as.data.table(sae.rate)
# SAE rate from: ...
sae.mortality <- readRDS("Data/sae.mortality.rds") # this is also required
sae.mortality <- as.data.table(sae.mortality)
# SAE mortality data from: ...


# Creating a vector of state names
state.names <- c("p.sus",	"p.sus.notest", "p.sus.nf",	"p.sus.nbt",	"p.sus.nct",	"p.sus.tc",
                 "p.sus.sae",	"p.sus.sae.death",
                 "p.sus.no.risk",
                 "p.ltbi", "p.ltbi.notest",	"p.ltbi.nf",	"p.ltbi.nbt",	"p.ltbi.nct",	"p.ltbi.tc",
                 "p.ltbi.sae",	"p.ltbi.sae.death",
                 "p.ltbi.ongoing.risk", "p.ltbi.no.risk",
                 "p.tb",	"p.tbr",	"p.tb.death",	"p.death",	"p.emigrate")


# Number of states
state.number <- length(state.names)

# a hack to manage the flows, state.cost, flow.cost and state.qaly values.
new.state.names <- c(state.names, paste("V.", state.names, sep = ""),
                     paste("SC.", state.names, sep = ""),
                     paste("FC.", state.names, sep = ""),
                     paste("SQ.", state.names, sep = ""))

unevaluated.flow.cost <- lazy(c(0, 0, param$TESTC, param$TESTC + param$ATTENDCOST, param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST, param$TESTC + param$ATTENDCOST + param$TREATC,
                                param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 0,
                                0,
                                0, 0, param$TESTC, param$TESTC + param$ATTENDCOST, param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST, param$TESTC + param$ATTENDCOST + param$TREATC,
                                param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 0,
                                0, 0,
                                0, 0, 0, 0, 0))

unevaluated.state.cost <- lazy(c(0, 0, 0, 0, 0, 0,
                                 0, 0,
                                 0,
                                 0, 0, 0, 0, 0, 0,
                                 0, 0,
                                 0, 0,
                                 param$TBCOST, 0, 0, 0, 0))

unevaluated.state.utility <- lazy(c(0, 0, 0, 0, 0, 0,
                                    0, 0,
                                    0,
                                    0, 0, 0, 0, 0, 0,
                                    0, 0,
                                    0, 0,
                                    param$TBCOST, 0, 0, 0, 0))

#Sample commands demonstrating the functional argument list.

arglist <- CreateArgumentList(state.names, state.number)

# BASELINE.S1.TM
# # manually create list of values ()
# list.values <- c(0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$RR * param$RRADJUST),	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR * param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR * param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$TBMR),	0,	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1)
# arglist$update.list(list.values) # For passing a entire list
# arglist$add.state.name(state.names)
# # saveRDS(S1.TMKD,file = "Data/BASELINE.S1.TM.rds")
# arglist$save.list("BASELINE.S1.TMKD")

# 
# S1.TM
# manually create list of values ()
# list.values <- c(0,	quote(1 - param$POP),	quote(param$POP * (1 - (1-param$TESTSP) * param$ATTEND)),	quote(param$POP * (1 - param$TESTSP) * param$ATTEND * (1 - param$BEGINTREAT)),	quote(param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)),	quote(param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$TREATR),	quote(param$POP * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	quote(param$SAEMR),	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote((1 - param$POP) * (1 - (param$RR * param$RRADJUST))),	quote(param$POP * (1 - (param$TESTSN * param$ATTEND) - (param$RR * param$RRADJUST * (1 - ((param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT)))))),	quote(param$POP * param$TESTSN * param$ATTEND * (1 - param$BEGINTREAT)),	quote(param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)),	quote(param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR),	quote(param$POP * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	0,	0,	quote(((1 - param$POP) * param$RR * param$RRADJUST) + (param$POP * param$RR * param$RRADJUST * (1 - ((param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT))))),	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$SAEMR),	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$TBMR),	0,	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1)
# arglist$update.list(list.values)
# arglist$add.state.name(state.names)
# # saveRDS(S1.TMKD,file = "Data/S1.TMKD.rds")
# arglist$save.list("S1.TMKD")

arglist.S1.TM <- arglist$load.list("S1.TMKD")
arglist.BASELINE.S1.TM <- arglist$load.list("BASELINE.S1.TMKD")

CreateStates(state.names) # instantiates a set of states objects with default values

# Defining additional strategy
S2 <- DefineStrategy(p.sus,	p.sus.notest, p.sus.nf,	p.sus.nbt,	p.sus.nct,	p.sus.tc,
                     p.sus.sae,	p.sus.sae.death,
                     p.sus.no.risk,
                     p.ltbi,	p.ltbi.notest, p.ltbi.nf,	p.ltbi.nbt,	p.ltbi.nct,	p.ltbi.tc,	
                     p.ltbi.sae,	p.ltbi.sae.death,
                     p.ltbi.ongoing.risk, p.ltbi.no.risk,
                     p.tb,	p.tbr,	p.tb.death,	p.death,	p.emigrate,
                     transition.matrix = do.call(DefineTransition, arglist.S1.TM))

# Defining baseline strategy
S0_12 <- DefineStrategy(p.sus,	p.sus.notest, p.sus.nf,	p.sus.nbt,	p.sus.nct,	p.sus.tc,
                        p.sus.sae,	p.sus.sae.death,
                        p.sus.no.risk,
                        p.ltbi,	p.ltbi.notest, p.ltbi.nf,	p.ltbi.nbt,	p.ltbi.nct,	p.ltbi.tc,	
                        p.ltbi.sae,	p.ltbi.sae.death,
                        p.ltbi.ongoing.risk, p.ltbi.no.risk,
                        p.tb,	p.tbr,	p.tb.death,	p.death,	p.emigrate,
                        transition.matrix = do.call(DefineTransition, arglist.BASELINE.S1.TM))

# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               # TBMR = 0.001,
                               RRADJUST = rradj,
                               # RRADJUST takes into account the fact that a proportion (10% in Victoria)
                               # of TB cases are picked up each year with existing TB control strategies, i.e.
                               # during follow-up as a result of an abnormal CXR during pre-migration off-shore screening.
                               BEGINTREAT = begintrt,
                               # Yet to finalise this parameter. i need to Work out if the chance of beginning treatment 
                               # is age-dependent. May well depend on treatment as well??
                               # TBFOLLOWUPADJUST = Get.TBFOLLOWUPADJUST(DT, year, treatment),
                               ATTEND = att,
                               # Proportion of migrants referred following off-shore screening (CXR) 
                               # that attend follow-up appointment once onshore. 
                               # Source: Flynn MG, Brown LK. Treatment of latent tuberculosis in migrants 
                               # to Victoria. Commun Dis Intell Q Rep 2015; 39(4): E578-83.
                               TIMETOTREAT = Get.TIMETOTREAT(S = "yearfraction", treatment),
                               # TIMETOTREAT takes into account the fact that having follow-up and LTBI
                               # treatment in the first year after migration will take time and so a migrant's
                               # chance of reactivating will remain for a period of time before their 
                               # follow-up and treatment process is complete. The time that they remain 
                               # at risk will be dependent on the treatment regimen (see timetotreat.dt).
                               SAE = Get.SAE(DT, treatment),
                               SAEMR = Get.SAEMR(DT, treatment),
                               EMIGRATE = Get.EMIGRATE(DT, year),
                               TESTSN = Get.TEST(S = "SN", testing),
                               TESTSP = Get.TEST(S = "SP", testing),
                               TESTC = Get.TEST(S = "cost.primary", testing),
                               TREATR = Get.TREAT(S = "rate", treatment),
                               TREATC = Get.TREAT(S = "cost.primary", treatment),
                               POP = Get.POP(DT, strategy),
                               UTILITY = Get.UTILITY(treatment),
                               ATTENDCOST = cattend,
                               PARTIALTREATCOST = Get.TREAT(S = "cost.partial", treatment),
                               TBCOST = ctb,
                               SAECOST = csae
)

# Uses aust.rds file to create a sample input
pop.master <- CreatePopulationMaster()
pop.master <- subset(pop.master, AGERP == 20 & ISO3 == "200+")



# This is only for test purposes...
Num_SIm <- 5
simdata <- simdata[1:Num_SIm, ]
simdata <- as.data.table(simdata)

# To run the model each of the parameters needs
# to be in the environment, i.e. not defined within
# another function
# so the following loop takes each row of 
# parameter values from the simdata table
# and runs the model

# I need to place the results from the 
# model runs into a table... here it is:
simrun.output <- c(seq(1, Num_SIm))
simrun.output <- as.data.frame(simrun.output)
simrun.output <- as.data.table(simrun.output)

# this loops over the number of simulations
# and runs the model
for(i in 1:Num_SIm) {
  simnumber <- i
  PSA <- 1
  begintrt <- simdata[simnumber, begintrt]
  att <- simdata[simnumber, att]
  rradj <- simdata[simnumber, rradj]
  cattend <- simdata[simnumber, cattend]
  csae <- simdata[simnumber, csae]
  cscreenqft <- simdata[simnumber, cscreenqft]
  cscreentst <- simdata[simnumber, cscreentst]
  ctb <- simdata[simnumber, ctb]
  ctreat3HP <- simdata[simnumber, ctreat3HP]
  ctreat4R <- simdata[simnumber, ctreat4R]
  ctreat6H <- simdata[simnumber, ctreat6H]
  ctreat9H <- simdata[simnumber, ctreat9H]
  cparttreat3HP <- simdata[simnumber, cparttreat3HP]
  cparttreat4R <- simdata[simnumber, cparttreat4R]
  cparttreat6H <- simdata[simnumber, cparttreat6H]
  cparttreat9H <- simdata[simnumber, cparttreat9H]
  # snqftgit <- simdata[simnumber, snqftgit]
  sntst10 <- simdata[simnumber, sntst10]
  sntst15 <- simdata[simnumber, sntst15]
  spqftgit <- simdata[simnumber, spqftgit]
  sptst10 <- simdata[simnumber, sptst10]
  sptst15 <- simdata[simnumber, sptst15]
  treatr3HP <- simdata[simnumber, treatr3HP]
  treatr4R <- simdata[simnumber, treatr4R]
  treatr6H <- simdata[simnumber, treatr6H]
  treatr9H <- simdata[simnumber, treatr9H]
  ttt3HP <- simdata[simnumber, ttt3HP]
  ttt4R <- simdata[simnumber, ttt4R]
  ttt6H <- simdata[simnumber, ttt6H]
  ttt9H <- simdata[simnumber, ttt9H]
  uactivetb <- simdata[simnumber, uactivetb]
  uactivetbr <- simdata[simnumber, uactivetbr]
  uhealthy <- simdata[simnumber, uhealthy]
  ultbi3HP <- simdata[simnumber, ultbi3HP]
  ultbi4R <- simdata[simnumber, ultbi4R]
  ultbi6H <- simdata[simnumber, ultbi6H]
  ultbi9H <- simdata[simnumber, ultbi9H]
  ultbipart3HP <- simdata[simnumber, ultbipart3HP]
  ultbipart4R <- simdata[simnumber, ultbipart4R]
  ultbipart6H <- simdata[simnumber, ultbipart6H]
  ultbipart9H <- simdata[simnumber, ultbipart9H]
  ultbitreatsae <- simdata[simnumber, ultbitreatsae]
  # Create a sample data table of test sensitivity & specificity
  tests.dt <- data.table(tests = c("QTFGIT", "TST10", "TST15"), 
                         SN = c(snqftgit, sntst10, sntst15),
                         SP = c(spqftgit, sptst10, sptst15),
                         # Sensitivity and specificity values from: Abubakar I, Drobniewski F, Southern J, et al. Prognostic value 
                         # of interferon-gamma release assays and tuberculin skin test in predicting the development of active 
                         # tuberculosis (UK PREDICT TB): a prospective cohort study. Lancet Infect Dis 2018; 18(10): 1077-87.
                         # cost.primary = c(74.34, 70.40, 70.40))
                         cost.primary = c(cscreenqft, cscreentst, cscreentst))
  # the line above reflects the fact that the costs of offshore screening are born by the migrant, not
  # Australia's health system
  
  # Create a sample treatment data table
  treatment.dt <- data.table(treatment = c("3HP","4R", "6H", "9H"),
                             rate = c(treatr3HP, treatr4R, treatr6H, treatr9H),
                             cost.primary = c(ctreat3HP, ctreat4R, ctreat6H, ctreat9H),
                             cost.partial = c(cparttreat3HP, cparttreat4R,
                                              cparttreat6H, cparttreat9H))
  
  # This data table indicates when those who receive LTBI treatment in the first 
  # year after migration are likely to have received that treatment (as an annual proportion).
  timetotreat.dt <- data.table(treatment = c("3HP", "4R", "6H", "9H"),
                               yearfraction = c(ttt3HP, ttt4R, ttt6H, ttt9H))
  # could talk to Michael Flynn to establish how long it takes to complete treatment
  
  # Create a sample utility data table
  # TODO: fix hard coded data table. It should take state.names and create the columns.
  utility.dt <- data.table(treatment = c("", "3HP", "4R", "6H", "9H"))
  utility.dt[, c(state.names) := as.numeric(NA)]
  
  utility.dt[treatment == "3HP", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart3HP, ultbi3HP,
                                                     ultbitreatsae, 0,
                                                     uhealthy,
                                                     uhealthy, uhealthy, uhealthy, uhealthy, ultbipart3HP, ultbi3HP,
                                                     ultbitreatsae, 0,
                                                     uhealthy, uhealthy,
                                                     uactivetb, uactivetbr, 0, 0, 0)]
  
  utility.dt[treatment == "4R", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
                                                    ultbitreatsae, 0,
                                                    uhealthy,
                                                    uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
                                                    ultbitreatsae, 0,
                                                    uhealthy, uhealthy,
                                                    uactivetb, uactivetbr, 0, 0, 0)]
  
  utility.dt[treatment == "6H", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart6H, ultbi6H,
                                                    ultbitreatsae, 0,
                                                    uhealthy,
                                                    uhealthy, uhealthy, uhealthy, uhealthy, ultbipart6H, ultbi6H,
                                                    ultbitreatsae, 0,
                                                    uhealthy, uhealthy,
                                                    uactivetb, uactivetbr, 0, 0, 0)]
  
  utility.dt[treatment == "9H", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart9H, ultbi9H,
                                                    ultbitreatsae, 0,
                                                    uhealthy,
                                                    uhealthy, uhealthy, uhealthy, uhealthy, ultbipart9H, ultbi9H,
                                                    ultbitreatsae, 0,
                                                    uhealthy, uhealthy,
                                                    uactivetb, uactivetbr, 0, 0, 0)]
  
  utility.dt[treatment == "", c(state.names) := .(uhealthy, uhealthy, NA, NA, NA, NA,
                                                  NA, NA, 
                                                  uhealthy,
                                                  uhealthy, uhealthy, NA, NA, NA, NA,
                                                  NA, NA,
                                                  uhealthy, NA,
                                                  uactivetb, uactivetbr, 0, 0, 0)]
  
  base <- DoRunModel(S0_12, start.year, cycles)
  base <- unlist(base)
  simrun.output[simnumber, basecost := base[1]]
  simrun.output[simnumber, baseqaly := base[1]]
  
  strat <- DoRunModel(S2, start.year, cycles)
  strat <- unlist(strat)
  simrun.output[simnumber, stratcost := strat[1]]
  simrun.output[simnumber, stratqaly := strat[1]] 
  
}




















# # Plot of PSA results on cost effectiveness plane.
# # The code below will plot the 10,000 model run outputs on a
# # cost effectiveness plane.
# # A blue willingness to pay line is drawn on the plane too
# # and the colour of the simulations will be either green or red
# # depending on whether the ICER value is under or over the WTP.
# 
# WTP = 1000 # willingness to pay threshold
# WTP_compare1 = 500
# 
# simdata$model = WTP * simdata$Effect_prop_diff 
# 
# simdata$model_true = simdata$model - simdata$cost_diff 
# 
# simdata$CE = ifelse(test = simdata$model_true > 0,yes = 1, no = 0 )
# 
# simdata$CE_col = ifelse(test = simdata$CE == 0, yes = 2, no = 3 )
# table(simdata$CE)
# 
# plot(simdata$cost_diff ~ simdata$Effect_prop_diff, 
#      col = simdata$CE_col, cex = .8, pch = 3,
#      xlim = c(-30, 30), ylim = c(-8000, 8000))
# abline(h = 0, lwd = 2 )
# abline(v = 0, lwd = 2 )
# abline(c(0, WTP), col = 4, lwd = 3)
# 
# # #abline(c(0,WTP_compare1), lwd=3)
# # table(simdata$CE)
# 
# 
# # Plot of acceptability curve
# # work out the proportion cost-effective
# icerlist <- simdata$icer
# maxwtp <- 100000
# wtp <- c(0:maxwtp)
# propcosteffectivefunc <- function(wtp){
#   (sum(icerlist < wtp)/10000) * 100
# }
# propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))
# 
# # create a new table, the accepty table 
# accepty <- data.frame(wtp, propcosteffect)
# 
# # plot
# myplot1 <- 
#   ggplot(accepty, aes(x = wtp, y = propcosteffect))+
#   geom_line(size = 1, colour = "blue") +
#   labs(x = "Willingness-to-pay threshold (AUD$)", 
#        y = "Proportion cost-effective (%)") +
#   scale_y_continuous(breaks = seq(0, 100, 20)) +
#   scale_x_continuous(label = comma, breaks = seq(0, 150000, 20000)) +
#   theme_bw() +
#   theme(text = element_text(size = 25))
#         # panel.border = element_blank(),
#         # legend.position = "right",
#         # axis.text.x = element_text(angle = 45, hjust = 1))
#         # axis.text.y = element_text(size = 12))
#         # panel.grid.major = element_blank(),
#         # panel.grid.minor = element_blank(),
# 
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
# tiff('acceptability.tiff', 
#      units = "in", width = 15, height = 12,
#      res = 600)
# myplot1
# dev.off()




# # Exploring density plots
# # Beta distribution
# # https://stephens999.github.io/fiveMinuteStats/beta.html
# # where a = b
# 
# p = seq(0,1, length=100)
# plot(p, dbeta(p, 100, 100), ylab = "density", type = "l", col = 4)
# lines(p, dbeta(p, 10, 10), type = "l", col = 3)
# lines(p, dbeta(p, 2, 2), col = 2) 
# lines(p, dbeta(p, 1, 1), col = 1) 
# legend(0.7,8, c("Be(100,100)","Be(10,10)","Be(2,2)", "Be(1,1)"),
#        lty = c(1,1,1,1),
#        col = c(4,3,2,1))
# 
# # where a != b
# p = seq(0,1, length=100)
# 
# dev.off()
# 
# plot(p, dbeta(p, 87.49806, 87410.56493), ylab = "density", type = "l", col = 4, xlim = c(0, 0.02))
# 
# plot(p, dbeta(p, 90.92, 82545.55), ylab="density", type = "l", col = 4, xlim = c(0, 0.02))
# 
# lines(p, dbeta(p, 90, 10), type ="l", col = 3)
# lines(p, dbeta(p, 30, 70), col = 2) 
# lines(p, dbeta(p, 3, 7), col = 1) 
# legend(0.2, 30, c("90.92,82545.55", "Be(90,10)", "Be(30, 70)", "Be(3, 7)"),
#        lty = c(1,1,1,1),col=c(4,3,2,1))
# 
# dist <- dbeta(p, 90.92,82545.55)
# dist