# PARAMETER ADJUSTMENT (now all in one place, i.e. below)

disc <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
startyear <- 2020 # start.year
start.year <- startyear
totalcycles <- 10  # cycles ... The mortality data continues until 2100 and migrant 
# inflows are possible until 2050
finalyear <- startyear + totalcycles
final.year <- finalyear
# The tests and treatments I want to consider in the run
testlist <- c("QTFGIT") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("3HP") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# MIGRANT INFLOWS
# the migrant inflow will stop after the following Markov cycle
finalinflow <- 0

# TRANSITIONS
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

targetfunc <- function(DT) {
  # 200+
  # DT <- subset(DT, ISO3 == "200+")
  # 150+
  # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" )
  # 100+
  # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149")
  # 40+
  DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149" | ISO3 == "40-99")
  # Adjust age
  DT <- subset(DT, AGERP > 10 &
                 AGERP < 66)
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





# Sensitivity and Specificity of screening tools
snqftgit <- 0.6104 # QFTGIT sensitivity baseline 0.6104, low 0.4925, high 0.7195
sntst10 <- 0.7532 # TST10 sensitivity baseline 0.6591, low 0.6418, high 0.8444
sntst15 <- 0.6753 # TST15 sensitivity baseline 0.6341, low 0.5590, high 0.7777
spqftgit <- 0.95820 # QFTGIT specificity baseline 0.95820, low 0.95700, high 0.95948
sptst10 <- 0.82227 # TST10 specificity baseline 0.82227, low 0.81780, high 0.92686
sptst15 <- 0.95117 # TST15 specificity baseline 0.95117, low 0.94978, high 0.95255
  
# Effectiveness of LTBI treatment (TREATR)
treatr3HP <- 0.74088 # TREATR 3HP baseline 0.74088, low 0.5, high 0.9
treatr4R <- 0.82272 # TREATR 4R baseline 0.82272, low , high
treatr6H <- 0.5568 # TREATR 6H baseline 0.5568, low , high
treatr9H <- 0.735800 # TREATR 9H baseline 0.735800, low , high

rradj <- 0.9 # RRADJUST baseline 0.9, lower 0.952, upper 0.875
begintrt <- 0.596 # BEGINTREAT baseline 0.596, lower 0.331, upper 0.70
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


att <- 0.836 # ATTEND baseline 0.836, lower , upper

# Time to complete LTBI treatment
ttt3HP <- 0.375 # TREATR 3HP baseline 0.375, low 0.292, high 0.500
ttt4R <- 0.458 # TREATR 4R baseline 0.458, low 0.375, high 0.583
ttt6H <- 0.625 # TREATR 6H baseline 0.625, low 0.542, high 0.750
ttt9H <- 0.875 # TREATR 9H baseline 0.875, low 0.792, high 1.00



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


# Look up the SAE mortality rate from sae.mortality (age and treatment dependent)
Get.SAEMR <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGERP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  DT$treatment <- as.character(treat)
  
  sae.mortality[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
  # sae.mortality[DT[, .(AGERP, treatment)], low, on = .(Age = AGERP, treatment = treatment)]
  # sae.mortality[DT[, .(AGERP, treatment)], high, on = .(Age = AGERP, treatment = treatment)]
  
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
uactivetb <- 0.780730111 # utility for active TB state baseline 0.780730111, low 0.7, high 0.8
uactivetbr <- 0.876 # utility for recovered TB state baseline 0.876, low , high
ultbitreatsae <- 0.8343333 # utility of SAE baseline 0.8343333, low 0.7, high 0.87
# 3HP
ultbi3HP <- 0.876 # utility for 3HP LTBI treatment baseline 0.876, low 0.8725, high
ultbipart3HP <- 0.876 # utility for 3HP partial LTBI treatment baseline 0.876, low 0.884166667, high
# 4R
ultbi4R <- 0.876 # utility for 4R LTBI treatment baseline 0.876, low , high
ultbipart4R <- 0.876 # utility for 4R partial LTBI treatment baseline 0.876, low , high
# 6H
ultbi6H <- 0.876 # utility for 6H LTBI treatment baseline 0.876, low , high
ultbipart6H <- 0.876 # utility for 6H partial LTBI treatment baseline 0.876, low , high
# 9H
ultbi9H <- 0.876 # utility for 9H LTBI treatment baseline 0.876, low , high
ultbipart9H <- 0.876 # utility for 9H partial LTBI treatment baseline 0.876, low , high

# # Utilities assuming a decrement with 3HP LTBI treatment (3 months treatment and 9 months with the 12 month figure)
# # and varying utility values for active TB, depending on symptom length
# uhealthy <- 0.876 # utility for healthy state baseline 0.876, low , high
# uactivetb <- 0.780730111 # 6 MONTHS SYMPTOMS utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# uactivetb <- 0.802382333 # 4 MONTHS SYMPTOMS utility for active TB state baseline 0.802382333, low 0.7, high 0.8
# uactivetb <- 0.818152333 # 3 MONTHS SYMPTOMS utility for active TB state baseline 0.818152333, low 0.7, high 0.8
# uactivetb <- 0.780730111 # 2 MONTHS SYMPTOMS utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# uactivetb <- 0.780730111 # 1 MONTHS SYMPTOM utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# uactivetbr <- 0.876 # utility for recovered TB state baseline 0.876, low , high
# ultbitreatsae <- 0.8343333
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


# # Utilities assuming a decrement with LTBI treatment, but using the healthy values for
# # 3HP utilities when not on treatment and varying symptom length for active TB
# uhealthy <- 0.876 # utility for healthy state baseline 0.876, low , high
# # uactivetb <- 0.780730111 # 6 MONTHS SYMPTOMS utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# # uactivetb <- 0.802382333 # 4 MONTHS SYMPTOMS utility for active TB state baseline 0.802382333, low 0.7, high 0.8
# # uactivetb <- 0.818152333 # 3 MONTHS SYMPTOMS utility for active TB state baseline 0.818152333, low 0.7, high 0.8
# # uactivetb <- 0.833922333 # 2 MONTHS SYMPTOMS utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# uactivetb <- 0.847017333 # 1 MONTHS SYMPTOM utility for active TB state baseline 0.780730111, low 0.7, high 0.8
# uactivetbr <- 0.876 # utility for recovered TB state baseline 0.876, low , high
# ultbitreatsae <- 0.8343333
# # 3HP
# ultbi3HP <- 0.8725 # utility for 3HP LTBI treatment baseline 0.876, low , high
# ultbipart3HP <- 0.884166667 # utility for 3HP partial LTBI treatment baseline 0.876, low , high
# # 4R
# ultbi4R <- 0.86833 # utility for 4R LTBI treatment baseline 0.876, low , high
# ultbipart4R <- 0.88417 # utility for 4R partial LTBI treatment baseline 0.876, low , high
# # 6H
# ultbi6H <- 0 # utility for 6H LTBI treatment baseline 0.876, low , high
# ultbipart6H <- 0 # utility for 6H partial LTBI treatment baseline 0.876, low , high
# # 9H
# ultbi9H <- 0 # utility for 9H LTBI treatment baseline 0.876, low , high
# ultbipart9H <- 0 # utility for 9H partial LTBI treatment baseline 0.876, low , high



# # COSTS - LTBI screening off-shore - i.e. one specialist appointment
cscreenqft <- 0 # cost of screening (tests.dt) baseline 0, high 94.69
cscreentst <-  0 # cost of screening (tests.dt) baseline 0, high 70.40

# cattend <- 143.18 # cost of attending first follow up appointment if it is with a specialist
cattend <- 103.48 # cost of attending first follow up appointment if it is with a GP

ctreat3HP <- 208.03 # cost of ltbi treatment (treatment.dt) baseline 350.71, low 298.15, high 560.95
cparttreat3HP <- 82.81 # cost of ltbi treatment (treatment.dt) baseline 225.74, low 208.22, high 295.82

ctreat4R <- 431.41 # cost of ltbi treatment (treatment.dt) baseline 570.79, low , high
cparttreat4R <- 157.82 # cost of ltbi treatment (treatment.dt) baseline 299.10, low , high

ctreat6H <- 239.53 # cost of ltbi treatment (treatment.dt) baseline 396.11, low , high
cparttreat6H <- 97.65 # cost of ltbi treatment (treatment.dt) baseline 233.29, low , high

ctreat9H <- 342.79 # cost of ltbi treatment (treatment.dt) baseline 500.64, low , high
cparttreat9H <- 138.23 # cost of ltbi treatment (treatment.dt) baseline 288.74, low , high


# # COSTS - LTBI screening on-shore: assuming GPs provide all screening and treatment
# cscreenqft <- 94.69 # 94.69 # cost of screening (tests.dt) baseline 0, high
# cscreentst10 <-  0 # cost of screening (tests.dt) baseline 0, high
# cscreentst15 <- 0 # cost of screening (tests.dt) baseline 0, high
# 
# cattend <- 38.20 # cost of attending first follow up appointment (NA, see above)
# 
# ctreat3HP <- 235.11 # cost of ltbi treatment (treatment.dt) baseline 350.71, low 298.15, high 560.95
# ctreat4R <- 476.19 # cost of ltbi treatment (treatment.dt) baseline 570.79, low , high
# ctreat6H <- 0 # cost of ltbi treatment (treatment.dt) baseline 396.11, low , high
# ctreat9H <- 0 # cost of ltbi treatment (treatment.dt) baseline 500.64, low , high
# cparttreat3HP <- 96.35 # cost of ltbi treatment (treatment.dt) baseline 225.74, low 208.22, high 295.82
# cparttreat4R <- 180.21 # cost of ltbi treatment (treatment.dt) baseline 299.10, low , high
# cparttreat6H <- 0 # cost of ltbi treatment (treatment.dt) baseline 233.29, low , high
# cparttreat9H <- 0 # cost of ltbi treatment (treatment.dt) baseline 288.74, low , high


ctb <- 12000 # TBCOST baseline 12550.52, low 8000, high 15000 ...10941

csae <- 1124 # SAECOST cost of severe adverse event resulting in hospitalisation

