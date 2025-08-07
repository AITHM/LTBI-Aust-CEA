# parameter_values.R

# Load required packages
library(readxl)
library(dplyr)

# Load parameters from Excel
param_file <- "parameters.xlsx"
params <- read_excel(param_file, sheet = "cascade_of_care")
switches <- read_excel(param_file, sheet = "switches")
costs <- read_excel(param_file, sheet = "costs")
utility <- read_excel(param_file, sheet = "utilities")

# Define lookup functions
get_param <- function(name) {
  val <- params$mid[params$p == name]
  if (length(val) == 0) stop(paste("Parameter not found:", name))
  return(val)
}

get_costs <- function(name) {
  val <- costs$mid[costs$p == name]
  if (length(val) == 0) stop(paste("Parameter not found:", name))
  return(val)
}

get_utility <- function(name) {
  val <- utility$mid[utility$p == name]
  if (length(val) == 0) stop(paste("Parameter not found:", name))
  return(val)
}
get_switch <- function(name) {
  if (!(name %in% switches$name)) {
    stop(paste("Switch not found:", name))
  }
  switches$value[switches$name == name]
}


# Load switches
onshore <- as.numeric(get_switch("onshore"))
emigration <- as.numeric(get_switch("emigration"))
payerperspect <- as.numeric(get_switch("payerperspect"))
disc <- as.numeric(get_switch("disc"))
startyear <- as.numeric(get_switch("startyear"))
totalcycles <- as.numeric(get_switch("totalcycles"))
finalyear <- startyear + totalcycles
kill.off.above <- as.numeric(get_switch("kill.off.above"))

migrant.inflow.size <- as.numeric(get_switch("migrant.inflow.size"))
finalinflow <- as.numeric(get_switch("finalinflow"))

testlist <- strsplit(get_switch("testlist"), ",")[[1]]
treatmentlist <- strsplit(get_switch("treatmentlist"), ",")[[1]]

# Cost and utility inputs
part.utility.dec <- 0.5
ultbipart3HP <- get_utility("uhealthy") - ((get_utility("uhealthy") - get_utility("ultbi3HP")) * part.utility.dec)
ultbipart4R <- get_utility("uhealthy") - ((get_utility("uhealthy") - get_utility("ultbi4R")) * part.utility.dec)
ultbipart6H <- get_utility("uhealthy") - ((get_utility("uhealthy") - get_utility("ultbi6H")) * part.utility.dec)
ultbipart9H <- get_utility("uhealthy") - ((get_utility("uhealthy") - get_utility("ultbi9H")) * part.utility.dec)

# Partial treatment cost ratios
part.appt <- 2
part.med <- 3

# Composite costs (using cascade parameters)
c.gp.first <- get_costs("c.gp.c.vr") * (1 - get_param("proportion.nonvr")) + get_costs("c.gp.c.nonvr") * get_param("proportion.nonvr")
c.gp.review <- get_costs("c.gp.b.vr") * (1 - get_param("proportion.nonvr")) + get_costs("c.gp.b.nonvr") * get_param("proportion.nonvr")

# Base costs
c.spec.first <- get_costs("c.spec.first")
c.spec.review <- get_costs("c.spec.review")
c.liver <- get_costs("c.liver")
prop.spec <- get_param("prop.spec")
chance.of.needing.mcs <- get_param("chance.of.needing.mcs")
c.mcs <- get_costs("c.mcs")
c.cxr <- get_costs("c.cxr")

# Attendance cost logic
if (onshore == 0) {
  cattend <- c.gp.first + (c.mcs * chance.of.needing.mcs) + c.cxr
} else {
  cattend <- ((c.gp.review + (c.mcs * chance.of.needing.mcs) + c.cxr) * (1 - prop.spec)) +
    ((c.spec.first + (c.mcs * chance.of.needing.mcs) + c.cxr) * prop.spec)
}

# Treatment regimens: 3HP, 4R, 6H, 9H
treatments <- list()
for (regimen in c("3HP", "4R", "6H", "9H")) {
  treatments[[regimen]] <- list(
    num_appt = get_param(paste0("num.appt", regimen)),
    med_cost = get_costs(paste0("cmed", regimen))
  )
  
  appt <- treatments[[regimen]]$num_appt * c.gp.review + ifelse(regimen %in% c("3HP", "6H", "9H"), c.liver, 0)
  spec_appt <- c.spec.first + (treatments[[regimen]]$num_appt - 1) * c.spec.review + ifelse(regimen %in% c("3HP", "6H", "9H"), c.liver, 0)
  
  assign(paste0("ctreat", regimen), appt + treatments[[regimen]]$med_cost)
  assign(paste0("cparttreat", regimen), appt / part.appt + treatments[[regimen]]$med_cost / part.med)
  assign(paste0("ctreatspec", regimen), spec_appt + treatments[[regimen]]$med_cost)
  assign(paste0("cparttreatspec", regimen), spec_appt / part.appt + treatments[[regimen]]$med_cost / part.med)
}

# Sensitivity analysis function
sensfunc <- function(paramname, loworhigh) {
  paramname <- deparse(substitute(paramname))
  colname <- deparse(substitute(loworhigh))
  newvalue <- params[params$p == paramname, ][[colname]]
  params[params$p == paramname, "mid"] <<- newvalue
}

# Set scientific notation off for readability
options(scipen = 999)

# Placeholder for sourced files if needed (e.g., Medical costs.R)
# Define the target population
if (onshore == 1) {
  Get.POP <- function(DT, strategy) {
    
    # 200+
    (ifelse(DT[, ISO3] == "200+", 1, 0)) & 
      # 150+
      # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) & 
      # 100+
      # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0)) &
      # 40+
      # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
      # Adjust age at arrival
      (ifelse(DT[, AGERP] > 10, 1, 0) &
         ifelse(DT[, AGERP] < 66, 1, 0))
    
  }
  
  targetfunc <- function(DT) {
    
    # 200+
    DT <- subset(DT, ISO3 == "200+")
    # 150+
    # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" )
    # 100+
    # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149")
    # 40+
    # DT <- subset(DT, ISO3 == "200+" | ISO3 == "150-199" | ISO3 == "100-149" | ISO3 == "40-99")
    # Adjust age at arrival
    DT <- subset(DT, AGERP > 10 &
                   AGERP < 66)
    DT
  }
  
} else if (onshore == 0) {
  
  Get.POP <- function(DT, strategy) {
    
    # 200+
    #(ifelse(DT[, ISO3] == "200+", 1, 0)) & 
    # 150+
    # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) & 
    # 100+
    (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0)) &
      # 40+
      # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
      # Adjust age at arrival
      (ifelse(DT[, AGERP] > 10, 1, 0) &
         ifelse(DT[, AGERP] < 66, 1, 0))
    
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
    # Adjust age at arrival
    DT <- subset(DT, AGERP > 10 &
                   AGERP < 66)
    DT
  }
}

options(scipen = 999)
params <- as.data.table(params)

#' Taking the values from the params table and
#' putting them into the environment
for(i in 1:nrow(params)) {
  assign(params[i, p], params[i, mid])
}

#' Adjusting the partial LTBI treatment utilities so 
#' they are dependent on the value of
#' the sampled utility for full treatment
part.utility.dec <- 0.5
ultbipart3HP <- uhealthy - ((uhealthy - ultbi3HP) * part.utility.dec)
ultbipart4R <- uhealthy - ((uhealthy - ultbi4R) * part.utility.dec)
ultbipart6H <- uhealthy - ((uhealthy - ultbi6H) * part.utility.dec)
ultbipart9H <- uhealthy - ((uhealthy - ultbi9H) * part.utility.dec)

# uactivetb <- uactivetb - (uhealthy - ultbi4R)

#' Sourcing the medical costs
costs <- read_excel(param_file, sheet = "costs")

#' These specify how much of the appointment and medicine
#' costs are applied for the partial costs and treatment
part.appt <- 2
part.med <- 3

c.gp.first <- c.gp.c.vr * (1 - proportion.nonvr) + c.gp.c.nonvr * proportion.nonvr

c.gp.review <- c.gp.b.vr * (1 - proportion.nonvr) + c.gp.b.nonvr * proportion.nonvr

chance.of.needing.mcs <- 0.1

#' Cost of initial appointment after positive screen
#' These costs are different for on and off-shore screening
#' so this need to be taken into account, i.e. for onshore
#' screening this appointment will be a review with the GP
#' or it may be the first appointment with a specialist
#' and a liver function test will be ordered
#' Also, all ongoing appointments related to LTBI treatment will be
#' review appointments

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

#' 3HP sort
appt <- num.appt3HP * c.gp.review + c.liver
spec.appt <- c.spec.first + (num.appt3HP - 1) * c.spec.review + c.liver
ctreat3HP <- appt + cmed3HP
cparttreat3HP <-  appt / part.appt + cmed3HP / part.med      
ctreatspec3HP <-  spec.appt + cmed3HP 
cparttreatspec3HP <-  spec.appt / part.appt + cmed3HP / part.med
#' 4r sort
appt <- num.appt4R * c.gp.review
spec.appt <- c.spec.first + (num.appt4R - 1) * c.spec.review
ctreat4R <- appt + cmed4R 
cparttreat4R <-  appt / part.appt + cmed4R / part.med      
ctreatspec4R <-  spec.appt + cmed4R 
cparttreatspec4R <-  spec.appt / part.appt + cmed4R / part.med
#' 6H sort
appt <- num.appt6H * c.gp.review + c.liver
spec.appt <- c.spec.first + (num.appt6H - 1) * c.spec.review + c.liver
ctreat6H <- appt + cmed6H
cparttreat6H <-  appt / part.appt + cmed6H / part.med      
ctreatspec6H <-  spec.appt + cmed6H 
cparttreatspec6H <-  spec.appt / part.appt + cmed6H / part.med
#' 9H sort
appt <- num.appt9H * c.gp.review + c.liver
spec.appt <- c.spec.first + (num.appt9H - 1) * c.spec.review + c.liver
ctreat9H <- appt + cmed9H
cparttreat9H <-  appt / part.appt + cmed9H / part.med      
ctreatspec9H <-  spec.appt + cmed9H 
cparttreatspec9H <-  spec.appt / part.appt + cmed9H / part.med 

#' Initial migrant cohort and LTBI prevalence and reactivation rates
aust <- readRDS("Data/Aust16.rds") # baseline
aust <- as.data.table(aust)
# aust <- subset(aust, ISO3 == "150-199")
#' Australian 2016 census data extracted from Table Builder by country of birth
#' (place of usual residence), single year age and single year of arrival. 

# #' Assuming a lower prevalence of LTBI and a higher reactivation rate (use UUI reactivation rate)
# aust[, LTBP := NULL]
# setnames(aust, "tfnum", "LTBP")

# #' Assuming a higher prevalence of LTBI and a lower reactivation rate (use LUI reactivation rate)
# aust[, LTBP := NULL]
# setnames(aust, "sfnum", "LTBP")

#' Reactivation rates
RRates <- readRDS("Data/RRatescobincidnosex.rds")
#' TB reactivation rate data from: Dale K, Trauer J, et al. Estimating long-term tuberculosis
#' reactivation rates in Australian migrants. Clinical Infectious Diseases 2019 (in press)
Get.RR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGERP, SEXP, YARP, ISO3, AGEP)])
  
  DT[ISO3 == "0-39" | ISO3 == "40-99", COBI := "<100"]
  
  DT[ISO3 == "100-149" | ISO3 == "150-199" | ISO3 == "200+", COBI := "100+"]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  
  ifelse(DT[, AGEP] > kill.off.above, 0,
         
         # Baseline reactivation rates
         RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], Rate, on = .(aaa = AGERP, Sex = SEXP,
                                                                           ysa = ST, cobi = COBI)]
         
         
         # # assuming a lower LTBI prevalence and a higher rate of reactivation
         # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], UUI, on = .(aaa = AGERP, Sex = SEXP,
         #                                                                   ysa = ST, cobi = COBI)]
         
         # # assuming a higher prevalence of LTBI and a lower rate of reactivation
         # RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], LUI, on = .(aaa = AGERP, Sex = SEXP,
         #                                                                  ysa = ST, cobi = COBI)]
  )
  
}


#' Reactivation rate adjustment for existing TB control
Get.RRADJ <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGERP, YARP, AGEP)])
  
  rradjrates[DT[, .(AGERP, ST = year - YARP)], rate, on = .(aaa = AGERP, ysa = ST)]
  
  # rradjrates[DT[, .(AGERP, ST = year - YARP)], lower, on = .(aaa = AGERP, ysa = ST)]
  
  # rradjrates[DT[, .(AGERP, ST = year - YARP)], upper, on = .(aaa = AGERP, ysa = ST)]
  
  # 1
  
}


#' Look up the mortality rate from vic.mortality
Get.MR <- function(xDT, year, rate.assumption = "Med") {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond the killing off age
  DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  vic.mortality[Age > kill.off.above, Prob := 1]
  
  vic.mortality[Year == year & mrate == rate.assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]
  
}

#' Look up TB mortality rate
Get.TBMR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond the killing off age
  DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  vic.tb.mortality[age > kill.off.above, Prob := 0]
  vic.tb.mortality[age > kill.off.above, lower := 0]
  vic.tb.mortality[age > kill.off.above, upper := 0]
  
  vic.tb.mortality[DT[, .(AGEP, SEXP)], Prob, on = .(age = AGEP, sex = SEXP)]
  # vic.tb.mortality[DT[, .(AGEP, SEXP)], lower, on = .(age = AGEP, sex = SEXP)]
  # vic.tb.mortality[DT[, .(AGEP, SEXP)], upper, on = .(age = AGEP, sex = SEXP)]
  
}


#' Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAE <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGEP)])
  
  # To lookup all ages beyond the killing off age
  DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  sae.rate[Age > kill.off.above, Rate := 0]
  sae.rate[Age > kill.off.above, low := 0]
  sae.rate[Age > kill.off.above, high := 0]
  
  DT$treatment <- as.character(treat)
  
  sae.rate[DT[, .(AGEP, treatment)], Rate, on = .(Age = AGEP, treatment = treatment)]
  # sae.rate[DT[, .(AGEP, treatment)], low, on = .(Age = AGEP, treatment = treatment)]
  # sae.rate[DT[, .(AGEP, treatment)], high, on = .(Age = AGEP, treatment = treatment)]
  
}

#' Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAEMR <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGEP)])
  
  # To lookup all ages beyond the killing off age
  DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  sae.mortality[Age > kill.off.above, Rate := 0]
  sae.mortality[Age > kill.off.above, low := 0]
  sae.mortality[Age > kill.off.above, high := 0]
  
  DT$treatment <- as.character(treat)
  
  sae.mortality[DT[, .(AGEP, treatment)], Rate, on = .(Age = AGEP, treatment = treatment)]
  # sae.mortality[DT[, .(AGEP, treatment)], low, on = .(Age = AGEP, treatment = treatment)]
  # sae.mortality[DT[, .(AGEP, treatment)], high, on = .(Age = AGEP, treatment = treatment)]
  
}

#' Emigrate rate from emigrate.rate (zero)
#' Source emigrate data
emigrate.rate <- readRDS("Data/emigrate.rate.rds") # BASELINE assumed rate incorporating both temp and permanent residents 
#' emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds") # LOWER assumed rate among permanent residents

emigrate.rate <- as.data.table(emigrate.rate)

#' Emigrate rate from emigrate.rate (age dependent)
Get.EMIGRATE <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGEP, YARP)])
  
  # To lookup all ages beyond the killing off age
  DT[AGEP > kill.off.above, AGEP := kill.off.above + 1]
  
  # Knocking everyone off after a certain age (mortality risk 100%, everything else 0)
  emigrate.rate[Age > kill.off.above, Rate := 0]
  emigrate.rate[Age > kill.off.above, lower := 0]
  emigrate.rate[Age > kill.off.above, upper := 0]
  
  if (emigration == 1) {
    
    emigrate.rate[DT[, .(AGEP)], Rate, on = .(Age = AGEP)] 
    # emigrate.rate[DT[, .(AGEP)], lower, on = .(Age = AGEP)]
    # emigrate.rate[DT[, .(AGEP)], upper, on = .(Age = AGEP)]
    
  } else if (emigration == 0) {
    
    0
    
  }
  
}


#' Look up treatment costs (it's treatment dependent)
Get.TREATC <- function(S, treat) {
  
  as.numeric(treatmentcost.dt[treatment == treat & practitioner == "spec", ..S]) * prop.spec +
    as.numeric(treatmentcost.dt[treatment == treat & practitioner == "gp", ..S]) * (1 - prop.spec)
  
}

#' Calculating the partial treatment efficacy
Get.PART.EFFIC <- function(xDT, C, E, treat) {
  
  DT <- copy(xDT[, .(year, AGEP)])
  
  treatment <- as.character(treat)
  
  treat.complete <- as.numeric(treatment.dt[treatment == treat, ..C])
  
  treat.effic <- as.numeric(treatment.dt[treatment == treat, ..E])
  
  ifelse(DT[, AGEP] > kill.off.above, 0,
         
         if (treat == '3HP') {
           
           treat.effic.1 <- 0
           treat.effic.2 <- 0.368 # Gao et al 2018
           treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
           
           ratio.1 <- 0.6993362 # Page and Menzies
           ratio.2 <- 0.3006638 # Page and Menzies
           
           PART.EFFIC <- treat.effic.2 * ratio.2
           
         } else if (treat == '4R') {
           
           treat.effic.1 <- 0
           treat.effic.2 <- 0.368 # Gao et al 2018
           treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
           
           ratio.1 <- 0.6993362 # Page and Menzies
           ratio.2 <- 0.3006638 # Page and Menzies
           
           PART.EFFIC <- treat.effic.2 * ratio.2
           
         } else if (treat == '6H') {
           
           treat.effic.1 <- 0
           treat.effic.2 <- 0.310 # IUAT
           treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
           
           ratio.1 <- 0.7273 # IUAT
           ratio.2 <- 0.2727 # IUAT
           
           PART.EFFIC <- treat.effic.2 * ratio.2 
           
         } else if (treat == '9H') {
           
           treat.effic.1 <- 0
           treat.effic.2 <- 0.310 # IUAT
           treat.effic.3 <- 0.69 # IUAT
           treat.effic.3 <- ifelse(treat.effic.3 >= treat.effic, treat.effic, treat.effic.3)
           
           ratio.1 <- 0.59259 # IUAT
           ratio.2 <- 0.18519 # IUAT
           ratio.3 <- 0.22222 # IUAT
           
           PART.EFFIC <- treat.effic.1 * ratio.1 +
             treat.effic.2 * ratio.2 +
             treat.effic.3 * ratio.3
           
         } else {
           
           PART.EFFIC <- 0
         }
  )
  
}

Get.FULL.EFFIC <- function(xDT, E, treat) {
  
  DT <- copy(xDT[, .(year, AGEP)])
  
  treatment <- as.character(treat)
  
  treat.effic <- as.numeric(treatment.dt[treatment == treat, ..E])
  
  ifelse(DT[, AGEP] > kill.off.above, 0, treat.effic)
  
}


Get.TREATR <- function(C, E, treat) {
  
  treat.complete <- as.numeric(treatment.dt[treatment == treat, ..C])
  
  treat.effic <- as.numeric(treatment.dt[treatment == treat, ..E])
  
  treatment <- as.character(treat)
  
  if (treat == '3HP') {
    
    treat.effic.1 <- 0
    treat.effic.2 <- 0.368 # Gao et al 2018
    treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
    
    ratio.1 <- 0.6993362 # Page and Menzies
    ratio.2 <- 0.3006638 # Page and Menzies
    
    treat.complete.1 <- (1 - treat.complete) * ratio.1
    treat.complete.2 <- (1 - treat.complete) * ratio.2
    
    TREATR <- treat.effic.1 * treat.complete.1 +
      treat.effic.2 * treat.complete.2 +
      treat.effic * treat.complete
    
  } else if (treat == '4R') {
    
    treat.effic.1 <- 0
    treat.effic.2 <- 0.368 # Gao et al 2018
    treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
    
    ratio.1 <- 0.6993362 # Page and Menzies
    ratio.2 <- 0.3006638 # Page and Menzies
    
    treat.complete.1 <- (1 - treat.complete) * ratio.1
    treat.complete.2 <- (1 - treat.complete) * ratio.2
    
    TREATR <- treat.effic.1 * treat.complete.1 +
      treat.effic.2 * treat.complete.2 +
      treat.effic * treat.complete
    
  } else if (treat == '6H') {
    
    treat.effic.1 <- 0
    treat.effic.2 <- 0.310 # IUAT
    treat.effic.2 <- ifelse(treat.effic.2 >= treat.effic, treat.effic, treat.effic.2)
    
    ratio.1 <- 0.7273 # IUAT
    ratio.2 <- 0.2727 # IUAT
    
    treat.complete.1 <- (1 - treat.complete) * ratio.1
    treat.complete.2 <- (1 - treat.complete) * ratio.2
    
    TREATR <- treat.effic.1 * treat.complete.1 +
      treat.effic.2 * treat.complete.2 +
      treat.effic * treat.complete
    
  } else if (treat == '9H') {
    
    treat.effic.1 <- 0
    treat.effic.2 <- 0.310 # IUAT
    treat.effic.3 <- 0.69 # IUAT
    treat.effic.3 <- ifelse(treat.effic.3 >= treat.effic, treat.effic, treat.effic.3)
    
    ratio.1 <- 0.59259 # IUAT
    ratio.2 <- 0.18519 # IUAT
    ratio.3 <- 0.22222 # IUAT
    
    treat.complete.1 <- (1 - treat.complete) * ratio.1
    treat.complete.2 <- (1 - treat.complete) * ratio.2
    treat.complete.3 <- (1 - treat.complete) * ratio.3
    
    TREATR <- treat.effic.1 * treat.complete.1 +
      treat.effic.2 * treat.complete.2 +
      treat.effic.3 * treat.complete.3 +
      treat.effic * treat.complete
    
  } else {
    
    TREATR <- 0
  }
  
}

