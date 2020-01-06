# PARAMETER ADJUSTMENT (now all in one place, i.e. below)

library(data.table)

# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
# onshore <- 1

params <- readRDS("params offshore.rds")
onshore <- 0
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
params <- as.data.table(params)

# This function can be used to choose which parameters to change
# for sensitivity analysis.
# It replaces the "mid" value in the dataframe with a 
# low or high value depending on what is specified.

# sensfunc <- function(paramname, loworhigh) {
#   paramname <- deparse(substitute(paramname))
#   colname <- deparse(substitute(loworhigh))
#   newvalue <- params[p == paramname, ..colname]
#   params[p == paramname, mid:= newvalue]
# }
#################################################################################################
 
# sensfunc(csae, high)

# params[p == "treatr4R", mid := 1]

# # alter treatr values, i.e. treatment completion and treatment efficacy separately
# sensfunc(treatr4R, low.treat.complete)
# sensfunc(treatr4R, high.treat.complete)
# sensfunc(treatr4R, low.treat.eff)
# sensfunc(treatr4R, high.treat.eff)

# # apply realistic LTBI utilities
# sensfunc(ultbi4R, low)
# sensfunc(ultbipart4R, low)
# sensfunc(ultbi3HP, low)
# sensfunc(ultbipart3HP, low)
# sensfunc(ultbi6H, low)
# sensfunc(ultbipart6H, low)
# sensfunc(ultbi9H, low)
# sensfunc(ultbipart9H, low)


# # # the perfect world - figure 8
# params[p == "treatr4R", mid := 1]
# params[p == "sntst10", mid := 1]
# params[p == "sptst10", mid := 1]
# params[p == "att", mid := 1]
# params[p == "begintrt", mid := 1]
# params[p == "begintrt", mid := 1]

#################################################################################################

# Target population
Get.POP <- function(DT, strategy) {
  
  # 200+
  # (ifelse(DT[, ISO3] == "200+", 1, 0)) & 
  # 150+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) & 
  # 100+
  (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0)) &
    # 40+
    # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
    # Adjust age
    (ifelse(DT[, AGERP] > 10, 1, 0) &
       ifelse(DT[, AGERP] < 36, 1, 0))
  
}

# Assigning other parameter values
disc <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
startyear <- 2020 # start.year
start.year <- startyear
totalcycles <- 30  # cycles ... The mortality data continues until 2100 and migrant 
# inflows are possible until 2050
finalyear <- startyear + totalcycles
final.year <- finalyear
# The tests and treatments I want to consider in the run
testlist <- c("TST15") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("4R") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# MIGRANT INFLOWS
# the migrant inflow will stop after the following Markov cycle
migrant.inflow.size <- 434340 # baseline 434340, permanent 103740
finalinflow <- 0

# Taking the values from the params table and
# putting them into the environment
for(i in 1:nrow(params)) {
  assign(params[i, p], params[i, mid])
}

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
                c.cxr) * (1 - prop.spec)) + 
    ((c.spec.first + (c.mcs * chance.of.needing.mcs) +
       c.cxr) * prop.spec)
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


# Testing the results for TST 5mm
# sntst10 <- 0.8077
# sptst10 <- 0.7005

# Initial migrant cohort and LTBI prevalence and reactivation rates
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
aust <- readRDS("Data/Aust16byTBincid.rds") # baseline
aust <- as.data.table(aust)
# aust <- subset(aust, ISO3 == "150-199")
# Australian 2016 census data extracted from Table Builder by country of birth
# (place of usual residence), single year age and single year of arrival. 

# # Assuming a lower prevalence of LTBI and a higher reactivation rate (use UUI reactivation rate)
# aust[, LTBP := NULL]
# setnames(aust, "tfnum", "LTBP")

# # Assuming a higher prevalence of LTBI and a lower reactivation rate (use LUI reactivation rate)
# aust[, LTBP := NULL]
# setnames(aust, "sfnum", "LTBP")

# Reactivation rates
Get.RR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGERP, SEXP, YARP, ISO3)])
  
  DT[ISO3 == "0-39" | ISO3 == "40-99", COBI := "<100"]  
  
  DT[ISO3 == "100-149" | ISO3 == "150-199" | ISO3 == "200+", COBI := "100+"]  
  
  DT[AGERP > 110, AGERP := 110]
  
  # Baseline reactivation rates
  RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], Rate, on = .(aaa = AGERP, Sex = SEXP,
                                                                    ysa = ST, cobi = COBI)]
  
  
  # # assuming a lower LTBI prevalence and a higher rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], UUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                   ysa = ST, cobi = COBI)]
  
  # # assuming a higher prevalence of LTBI and a lower rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], LUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                  ysa = ST, cobi = COBI)]
  
}


# Reactivation rate adjustment for existing TB control
Get.RRADJ <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGERP, YARP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  rradjrates[DT[, .(AGERP, ST = year - YARP)], rate, on = .(aaa = AGERP, ysa = ST)]
  
  # rradjrates[DT[, .(AGERP, ST = year - YARP)], lower, on = .(Age = AGERP, ysa = ST)]
  
  # rradjrates[DT[, .(AGERP, ST = year - YARP)], upper, on = .(Age = AGERP, ysa = ST)]
  
}



# Change the makeup of the migrant inflow by TB incidence in country of birth
# Altering the COBI for all those who have arrived from 
# a country with 100-149 to 40-99, i.e. mostly Vietnam
# aust <- as.data.table(aust)
# aust[ISO3 == "100-149", ISO3 := "40-99"]

# Look up the mortality rate from vic.mortality
Get.MR <- function(xDT, year, rate.assumption = "Med") {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond 110
  DT[AGEP > 100, AGEP := 100]
  
  vic.mortality[Year == year & mrate == rate.assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]
  
}

# Look up TB mortality rate
Get.TBMR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond 95 & 97
  DT[AGEP > 95 & SEXP == "Male", AGEP := 95]
  DT[AGEP > 97 & SEXP == "Female", AGEP := 97]
  DT[AGEP > 97 & SEXP == "Both", AGEP := 97]
  
  
  vic.tb.mortality[DT[, .(AGEP, SEXP)], Prob, on = .(age = AGEP, sex = SEXP)]
  # vic.tb.mortality[DT[, .(AGEP, SEXP)], lowerProb, on = .(age = AGEP, sex = SEXP)]
  # vic.tb.mortality[DT[, .(AGEP, SEXP)], upperProb, on = .(age = AGEP, sex = SEXP)]
  
}


# Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAE <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  sae.rate[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
  
}

# Emigrate rate from emigrate.rate (zero)
# Source emigrate data
emigrate.rate <- readRDS("Data/emigrate.rate.rds") # BASELINE assumed rate incorporating both temp and permanent residents 
# emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds") # LOWER assumed rate among permanent residents
emigrate.rate <- as.data.table(emigrate.rate)

# Emigrate rate from emigrate.rate (age dependent)
Get.EMIGRATE <- function(xDT, year) {

  DT <- copy(xDT[, .(year, AGERP, YARP)])

  DT[AGERP > 110, AGERP := 110]

  emigrate.rate[DT[, .(AGERP)], Rate, on = .(Age = AGERP)]
  # emigrate.rate[DT[, .(AGERP)], lower, on = .(Age = AGERP)]
  # emigrate.rate[DT[, .(AGERP)], upper, on = .(Age = AGERP)]

}

# Get.EMIGRATE <- function(xDT, year) {
# 
#   0
# 
# }


# Look up treatment costs (it's treatment dependent)
Get.TREATC <- function(S, treat) {
  
  as.numeric(treatmentcost.dt[treatment == treat & practitioner == "spec", ..S]) * prop.spec +
    as.numeric(treatmentcost.dt[treatment == treat & practitioner == "gp", ..S]) * (1 - prop.spec)
  
}

