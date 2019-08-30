# PARAMETER ADJUSTMENT (now all in one place, i.e. below)

disc <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
startyear <- 2020 # start.year
totalcycles <- 30  # cycles ... The mortality data continues until 2100 and migrant inflows are possible until 2050
finalyear <- startyear + totalcycles

# The tests and treatments I want to consider in the run
testlist <- c("TST15") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("3HP") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# MIGRANT INFLOWS
# the migrant inflow will stop after the following Markov cycle
finalinflow <- 0

# TRANSITIONS
# Target population
Get.POP <- function(DT, strategy) {
  
  ifelse(DT[, ISO3] == "200+", 1, 0) |
    ifelse(DT[, ISO3] == "150-199", 1, 0) |
    ifelse(DT[, ISO3] == "100-149", 1, 0) &
    # ifelse(DT[, ISO3] == "40-99", 1, 0) &
    ifelse(DT[, AGERP] > 10, 1, 0) &
    ifelse(DT[, AGERP] < 36, 1, 0)
  
}

targetfunc <- function(DT) {
  DT <- subset(DT, ISO3 == "200+" |
                 ISO3 == "150+" |
                 ISO3 == "100-149")
                 # ISO3 == "40-99")
  DT <- subset(DT, AGERP > 10 &
                 AGERP < 36)
  DT
}


# LTBI prevalence and reactivation rates
aust <- readRDS("Data/Aust16byTBincid.rds") # baseline
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
  
  # # Using upper uncertainty interval, i.e. assuming a higher rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], UUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                   ysa = ST, cobi = COBI)]
  
  # # # Using upper uncertainty interval, i.e. assuming a higher rate of reactivation
  # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], LUI, on = .(aaa = AGERP, Sex = SEXP,
  #                                                                  ysa = ST, cobi = COBI)]
  
}

# Sensitivity and Specificity of screening tools
snqftgit <- 0.6104 # QFTGIT sensitivity baseline 0.6104, low 0.4925, high 0.7195
sntst10 <- 0.6591 # TST10 sensitivity baseline 0.7532, low 0.6418, high 0.8444
sntst15 <- 0.6341 # TST15 sensitivity baseline 0.6753, low 0.5590, high 0.7777
spqftgit <- 0.7784 # QFTGIT specificity baseline 0.7784, low 0.7629, high 0.7886
sptst10 <- 0.6679 # TST10 specificity baseline 0.6679, low 0.6562, high 0.6796
sptst15 <- 0.7726 # TST15 specificity baseline 0.7726, low 0.7621, high 0.7829
  
# Effectiveness of LTBI treatment (TREATR)
treatr3HP <- 0.74088 # TREATR 3HP baseline 0.74088, low , high
treatr4R <- 0.82272 # TREATR 4R baseline 0.82272, low , high
treatr6H <- 0.5568 # TREATR 6H baseline 0.5568, low , high
treatr9H <- 0.735800 # TREATR 9H baseline 0.735800, low , high

rradj <- 0.9 # RRADJUST baseline 0.9, lower 0.952, upper 0.875
begintrt <- 0.7 # BEGINTREAT baseline 0.7, lower 0.331, upper 1
# Look up the chance of beginning treatment (age and treatment??? dependent)
# Get.BEGINTREAT <- function(xDT, year) {
#   
#   DT <- copy(xDT[, .(AGERP)])
#   
#   DT[AGERP > 110, AGERP := 110]
#   
#   begintreat.rate[DT[, .(AGERP)], Rate, on = .(Age = AGERP)]
#   
# }
# BEGINTREAT = Get.BEGINTREAT(DT, year),
att <- 0.836 # ATTEND baseline 0.836, lower , upper

# Time to complete LTBI treatment
ttt3HP <- 0.33 # TREATR 3HP baseline 0.33, low 0.25, high 0.50
ttt4R <- 0.42 # TREATR 4R baseline 0.42, low 0.33, high 0.58
ttt6H <- 0.58 # TREATR 6H baseline 0.58, low 0.50, high 0.75
ttt9H <- 0.83 # TREATR 9H baseline 0.83, low 0.75, high 1.0

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
  
}

# Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAE <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  sae.rate[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
  
}


# Look up the SAE mortality rate from sae.mortality (age and treatment dependent)
Get.SAEMR <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  sae.mortality[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
  
}

# Emigrate rate from emigrate.rate (zero)

Get.EMIGRATE <- function(xDT, year) {

  0

}


# # Emigrate rate from emigrate.rate (age dependent)
# Get.EMIGRATE <- function(xDT, year) {
# 
#   DT <- copy(xDT[, .(year, AGERP, YARP)])
# 
#   DT[AGERP > 110, AGERP := 110]
# 
#   emigrate.rate[DT[, .(AGERP)], Rate, on = .(Age = AGERP)]
# 
# }

# # Emigrate rate from emigrate.rate (age and source country dependent)
# Get.EMIGRATE <- function(xDT, year) {
#   
#   DT <- copy(xDT[, .(year, AGERP, YARP, ISO3)])
#   
#   DT[year - YARP > 2, VISA := "perm"]  
#   
#   DT[year - YARP < 3, VISA := "temp"]  
#   
#   DT[AGERP > 110, AGERP := 110]
#   
#   emigrate.rate[DT[, .(AGERP, ISO3, VISA)], Rate, on = .(Age = AGERP, VISA = VISA,
#                                                          ISO3 = ISO3)]
#   
# }

# UTILITIES
# no decrement for LTBI treatment
uhealthy <- 0.876 # utility for healthy state baseline 0.876, low , high
uactivetb <- 0.781 # utility for active TB state baseline 0.781, low 0.7, high 0.8
uactivetbr <- 0.876 # utility for recovered TB state baseline 0.876, low , high
ultbitreatsae <- 0.8468
# 3HP
ultbi3HP <- 0.876 # utility for 3HP LTBI treatment baseline 0.876, low , high
ultbipart3HP <- 0.876 # utility for 3HP partial LTBI treatment baseline 0.876, low , high
# 4R
ultbi4R <- 0.876 # utility for 4R LTBI treatment baseline 0.876, low , high
ultbipart4R <- 0.876 # utility for 4R partial LTBI treatment baseline 0.876, low , high
# 6H
ultbi6H <- 0.876 # utility for 6H LTBI treatment baseline 0.876, low , high
ultbipart6H <- 0.876 # utility for 6H partial LTBI treatment baseline 0.876, low , high
# 9H
ultbi9H <- 0.876 # utility for 9H LTBI treatment baseline 0.876, low , high
ultbipart9H <- 0.876 # utility for 9H partial LTBI treatment baseline 0.876, low , high

# # Utilities assuming a decrement with LTBI treatment
# uhealthy <- 0.876 # utility for healthy state baseline 0.876, low , high
# uactivetb <- 0.781 # utility for active TB state baseline 0.781, low 0.7, high 0.8
# uactivetbr <- 0.876 # utility for recovered TB state baseline 0.876, low , high
# ultbitreatsae <- 0.8468
# # 3HP
# ultbi3HP <- 0.835 # utility for 3HP LTBI treatment baseline 0.876, low , high
# ultbipart3HP <- 0.838 # utility for 3HP partial LTBI treatment baseline 0.876, low , high
# # 4R
# ultbi4R <- 0.835 # utility for 4R LTBI treatment baseline 0.876, low , high
# ultbipart4R <- 0.838 # utility for 4R partial LTBI treatment baseline 0.876, low , high
# # 6H
# ultbi6H <- 0.832 # utility for 6H LTBI treatment baseline 0.876, low , high
# ultbipart6H <- 0.837 # utility for 6H partial LTBI treatment baseline 0.876, low , high
# # 9H
# ultbi9H <- 0.827 # utility for 9H LTBI treatment baseline 0.876, low , high
# ultbipart9H <- 0.868 # utility for 9H partial LTBI treatment baseline 0.876, low , high

# COSTS
cscreenqft <- 0 # cost of screening (tests.dt) baseline 0, high 74.34
cscreentst10 <- 0 # cost of screening (tests.dt) baseline 0, high 70.40
cscreentst15 <- 0 # cost of screening (tests.dt) baseline 0, high 70.40

cattend <- 143.18 # cost of attending first follow up appointment

ctreat3HP <- 310.47 # cost of ltbi treatment (treatment.dt) baseline 310.47, high 
ctreat4R <- 568.45 # cost of ltbi treatment (treatment.dt) baseline 568.45, low , high 
ctreat6H <- 353.39 # cost of ltbi treatment (treatment.dt) baseline 353.39, low , high 
ctreat9H <- 436.56 # cost of ltbi treatment (treatment.dt) baseline 436.56, low , high 
parttreat <- 0.333333 # the proportion of the whole cost of treatment that is assumed
                      # to be incurred by a migrant that does not complete treatment
ctb <- 11538 # TBCOST cost of severe adverse event resulting in hospitalisation

csae <- 1124 # SAECOST cost of severe adverse event resulting in hospitalisation

