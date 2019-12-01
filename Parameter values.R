# PARAMETER ADJUSTMENT (now all in one place, i.e. below)

library(data.table)


# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
dt <- readRDS("params.rds")
dt <- as.data.table(dt)


# This function can be used to choose which parameters to change
# for sensitivity analysis.
# It replaces the "mid" value in the dataframe with a 
# low or high value depending on what is specified.

sensfunc <- function(paramname, loworhigh) { 
  paramname <- deparse(substitute(paramname))
  colname <- deparse(substitute(loworhigh))
  newvalue <- dt[p == paramname, ..colname]
  dt[p == paramname, mid:= newvalue]
}

# sensfunc(rradj, high)

# Taking the values from the params table and
# putting them into the environment
for(i in 1:nrow(dt)) {
  assign(dt[i, p], dt[i, mid])
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
testlist <- c("QTFGIT", "TST10", "TST15") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("4R", "3HP", "6H", "9H") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# MIGRANT INFLOWS
# the migrant inflow will stop after the following Markov cycle
finalinflow <- 0

proportion.needing.spec <- 0.135 # Loutet et al 2018 UK study
# prop.under35.needing.spec <- 0.05
# prop.over35.needing.spec <- 0.25

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

targetfunc <- function(DT) {
  # 200+
  # DT <- subset(DT, ISO3 == "200+")
  # 150+
  # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" )
  # 100+
  DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149")
  # 40+
  # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149" | ISO3 == "40-99")
  # Adjust age
  DT <- subset(DT, AGERP > 10 &
                 AGERP < 36)
  DT
}

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
  
  # # 10% lifetime reactivation rates
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], Rate+0.000455, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                   ysa = ST, cobi = COBI)]
  
  
  # # Using upper uncertainty interval, i.e. assuming a higher rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], UUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                   ysa = ST, cobi = COBI)]
  
  # # # Using upper uncertainty interval, i.e. assuming a higher rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], LUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                  ysa = ST, cobi = COBI)]
  
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
  
}

# Get.EMIGRATE <- function(xDT, year) {
#   
#   0
#   
# }


# Look up treatment costs (it's treatment dependent)
Get.TREATC <- function(S, treat) {
  
  as.numeric(treatmentcost.dt[treatment == treat & practitioner == "spec", ..S]) * proportion.needing.spec +
    as.numeric(treatmentcost.dt[treatment == treat & practitioner == "gp", ..S]) * (1 - proportion.needing.spec)
  
}

