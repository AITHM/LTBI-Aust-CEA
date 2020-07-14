
library(data.table)

# Cost calculations

# Sourcing the medical costs
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("Medical costs.R")

# Create table of parameters
p <- c("attscreen", "att", "cscreenqft", "cscreentst",
       "num.appt3HP", "num.appt4R", "num.appt6H", "num.appt9H",
       "begintrt", "prop.spec",
       "snqftgit", "spqftgit",
       "sntst15", "sptst15", "sntst10", "sptst10",
       "treat.complete.3HP", "treat.complete.4R", "treat.complete.6H", "treat.complete.9H",
       "treat.effic.3HP", "treat.effic.4R", "treat.effic.6H", "treat.effic.9H",
       "ttt3HP", "ttt4R", "ttt6H", "ttt9H", 
       "cattend", "csae", "ctb",
       "cmed3HP", "cmed4R", "cmed6H", "cmed9H",
       "num.appt1HP",
       "cmed1HP",
       "ctreat1HP",
       "ctreatspec1HP",
       "cparttreat1HP",
       "cparttreatspec1HP",
       "num.appt3HR",
       "cmed3HR",
       "ctreat3HR",
       "ctreatspec3HR",
       "cparttreat3HR",
       "cparttreatspec3HR",
       "num.appt6wP",
       "cmed6wP",
       "ctreat6wP",
       "ctreatspec6wP",
       "cparttreat6wP",
       "cparttreatspec6wP",
       "ctreat3HP", "cparttreat3HP", "ctreat4R", "cparttreat4R",  
       "ctreat6H", "cparttreat6H", "ctreat9H", 
       "cparttreat9H", "ctreatspec3HP", "cparttreatspec3HP", 
       "ctreatspec4R", "cparttreatspec4R", "ctreatspec6H", 
       "cparttreatspec6H", "ctreatspec9H", 
       "cparttreatspec9H", "uactivetb", "uactivetbr", 
       "uhealthy", "ultbi3HP", "ultbi4R",
       "ultbi6H", "ultbi9H", "ultbitreatsae",
       "ultbipart3HP", "ultbipart4R", "ultbipart6H", "ultbipart9H")
params <- data.frame(p)
params <- as.data.table(p)
params[, mid := 0]
params[, low := 0]
params[, high := 0]
params[, distribution := "pert"]
params[, shape := 4]

c.gp.first <- c.gp.c.vr * (1 - proportion.nonvr) + c.gp.c.nonvr * proportion.nonvr

c.gp.review <- c.gp.b.vr * (1 - proportion.nonvr) + c.gp.b.nonvr * proportion.nonvr

chance.of.needing.mcs <- 0.1

# Parameters that need to vary in sensitivity analysis 
# - medicine costs - uniform parameter???????? or fixed??????
# - percentage requiring specialist care
# - number of follow-up appointments required
# - number of extra assessments required
# - 
params[p == "prop.spec", mid := 0.135] 
params[p == "prop.spec", low := 0.085] 
params[p == "prop.spec", high := 0.185] 

########DON'T SKEW THE COSTS IN FAVOUR OF LOW COSTS FOR MEDICINES??????


chance.of.needing.mcs <- 0.1


# Cost of initial appointment after positive screen

prop.spec <- params[p == "prop.spec", mid] 

params[p == "cattend",
       mid := (c.gp.review + (c.mcs * chance.of.needing.mcs) +
                       c.cxr) * (1 - prop.spec) + 
         (c.spec.first + (c.mcs * chance.of.needing.mcs) +
            + c.cxr) * prop.spec]
params[p == "cattend",
       low := mid]
params[p == "cattend",
       high := mid]

# These specify how much of the appointment and medicine
# costs are applied for the partial costs and treatment
part.appt <- 2
part.med <- 3

# Cost of screening
params[p == "cscreenqft", mid := 113.48] 
params[p == "cscreenqft", low := 113.48] 
params[p == "cscreenqft", high := 113.48] 

params[p == "cscreentst", mid := 116.07] 
params[p == "cscreentst", low := 116.07] 
params[p == "cscreentst", high := 116.07] 


# These specify how much of the appointment and medicine
# costs are applied for the partial costs and treatment
part.appt <- 2
part.med <- 3


# Cost of 1HP latent TB treatment
inh.packets <- 1
rpt.packets <- 5

params[p == "num.appt1HP", mid := 0] 
params[p == "num.appt1HP", low := 0] 
params[p == "num.appt1HP", high := 1] 

appt.num.1HP <- params[p == "num.appt1HP", mid]

med.mid <- inh.packets * c.inh.mid + rpt.packets * c.rifapent.mid
med.low <- inh.packets * c.inh.low + rpt.packets * c.rifapent.low
med.high <- inh.packets * c.inh.high + rpt.packets * c.rifapent.high

params[p == "cmed1HP", mid := med.mid] 
params[p == "cmed1HP", low := med.low] 
params[p == "cmed1HP", high := med.high] 

med.cost.1HP <- params[p == "cmed1HP", mid]

appt <- (appt.num.1HP * c.gp.review) + c.liver

spec.appt <- appt.num.1HP * c.spec.review + c.liver

params[p == "ctreat1HP", mid := appt + med.cost.1HP] 

params[p == "cparttreat1HP",
       mid := appt / part.appt + med.cost.1HP / part.med] 

params[p == "ctreatspec1HP", mid := spec.appt + med.cost.1HP] 

params[p == "cparttreatspec1HP",
       mid := spec.appt / part.appt + med.cost.1HP / part.med] 


# Cost of 6wP, 6 weeks daily rifapentine latent TB treatment
rpt.packets <- 7

params[p == "num.appt6wP", mid := 1]
params[p == "num.appt6wP", low := 1]
params[p == "num.appt6wP", high := 2]

appt.num.6wP <- params[p == "num.appt6wP", mid]

med.mid <- rpt.packets * c.rifapent.mid
med.low <- rpt.packets * c.rifapent.low
med.high <- rpt.packets * c.rifapent.high

params[p == "cmed6wP", mid := med.mid]
params[p == "cmed6wP", low := med.low]
params[p == "cmed6wP", high := med.high]

med.cost.6wP <- params[p == "cmed6wP", mid]

appt <- (appt.num.6wP * c.gp.review) + c.liver

spec.appt <- appt.num.6wP * c.spec.review + c.liver

params[p == "ctreat6wP", mid := appt + med.cost.6wP]

params[p == "cparttreat6wP",
       mid := appt / part.appt + med.cost.6wP / part.med]

params[p == "ctreatspec6wP", mid := spec.appt + med.cost.6wP]

params[p == "cparttreatspec6wP",
       mid := spec.appt / part.appt + med.cost.6wP / part.med]


# Cost of 3HR latent TB treatment
inh.packets <- 1
rif.packets <- 2

params[p == "num.appt3HR", mid := 2] 
params[p == "num.appt3HR", low := 1] 
params[p == "num.appt3HR", high := 4] 

appt.num.3HR <- params[p == "num.appt3HR", mid]

med.mid <- inh.packets * c.inh.mid + rif.packets * c.rifamp.mid
med.low <- inh.packets * c.inh.low + rif.packets * c.rifamp.low
med.high <- inh.packets * c.inh.high + rif.packets * c.rifamp.high

params[p == "cmed3HR", mid := med.mid] 
params[p == "cmed3HR", low := med.low] 
params[p == "cmed3HR", high := med.high] 

med.cost.3HR <- params[p == "cmed3HR", mid]

appt <- (appt.num.3HR * c.gp.review) + c.liver

spec.appt <- appt.num.3HR * c.spec.review + c.liver

params[p == "ctreat3HR", mid := appt + med.cost.3HR] 

params[p == "cparttreat3HR",
       mid := appt / part.appt + med.cost.3HR / part.med] 

params[p == "ctreatspec3HR", mid := spec.appt + med.cost.3HR] 

params[p == "cparttreatspec3HR",
       mid := spec.appt / part.appt + med.cost.3HR / part.med]

# Cost of 3HP latent TB treatment
inh.packets <- 1
rpt.packets <- 3

params[p == "num.appt3HP", mid := 2] 
params[p == "num.appt3HP", low := 1] 
params[p == "num.appt3HP", high := 4] 

appt.num.3HP <- params[p == "num.appt3HP", mid]

med.mid <- inh.packets * c.inh.mid + rpt.packets * c.rifapent.mid
med.low <- inh.packets * c.inh.low + rpt.packets * c.rifapent.low
med.high <- inh.packets * c.inh.high + rpt.packets * c.rifapent.high

params[p == "cmed3HP", mid := med.mid] 
params[p == "cmed3HP", low := med.low] 
params[p == "cmed3HP", high := med.high] 

med.cost.3HP <- params[p == "cmed3HP", mid]

appt <- appt.num.3HP * c.gp.review + c.liver

spec.appt <- appt.num.3HP * c.spec.review + c.liver

params[p == "ctreat3HP", mid := appt + med.cost.3HP] 

params[p == "cparttreat3HP",
       mid := appt / part.appt + med.cost.3HP / part.med] 

params[p == "ctreatspec3HP", mid := spec.appt + med.cost.3HP] 

params[p == "cparttreatspec3HP",
       mid := spec.appt / part.appt + med.cost.3HP / part.med] 

# Cost of 4R latent TB treatment
rif.packets <- 3

params[p == "num.appt4R", mid := 2] 
params[p == "num.appt4R", low := 1] 
params[p == "num.appt4R", high := 4] 

appt.num.4R <- params[p == "num.appt4R", mid]

med.mid <- c.rifamp.mid * rif.packets
med.low <- c.rifamp.low * rif.packets
med.high <- c.rifamp.high * rif.packets

params[p == "cmed4R", mid := med.mid] 
params[p == "cmed4R", low := med.low] 
params[p == "cmed4R", high := med.high] 

med.cost.4R <- params[p == "cmed4R", mid]

appt <- appt.num.4R * c.gp.review

spec.appt <- appt.num.4R * c.spec.review

params[p == "ctreat4R", mid := appt + med.cost.4R] 

params[p == "cparttreat4R",
       mid := appt / part.appt + med.cost.4R / part.med] 

params[p == "ctreatspec4R", mid := spec.appt + med.cost.4R] 

params[p == "cparttreatspec4R",
       mid := spec.appt / part.appt + med.cost.4R / part.med] 


# Cost of 6H latent TB treatment
inh.packets <- 6

params[p == "num.appt6H", mid := 3] 
params[p == "num.appt6H", low := 2] 
params[p == "num.appt6H", high := 7] 

appt.num.6H <- params[p == "num.appt6H", mid]

med.mid <- inh.packets * c.inh.mid
med.low <- inh.packets * c.inh.low
med.high <- inh.packets * c.inh.high

params[p == "cmed6H", mid := med.mid] 
params[p == "cmed6H", low := med.low] 
params[p == "cmed6H", high := med.high] 

med.cost.6H <- params[p == "cmed6H", mid]

appt <- appt.num.6H * c.gp.review + c.liver

spec.appt <- appt.num.6H * c.spec.review + c.liver

params[p == "ctreat6H", mid := appt + med.cost.6H] 

params[p == "cparttreat6H",
       mid := appt / part.appt + med.cost.6H / part.med] 

params[p == "ctreatspec6H", mid := spec.appt + med.cost.6H] 

params[p == "cparttreatspec6H",
       mid := spec.appt / part.appt + med.cost.6H / part.med] 


# Cost of 9H latent TB treatment
inh.packets <- 9

params[p == "num.appt9H", mid := 4] 
params[p == "num.appt9H", low := 3] 
params[p == "num.appt9H", high := 8] 

appt.num.9H <- params[p == "num.appt9H", mid]

med.mid <- inh.packets * c.inh.mid
med.low <- inh.packets * c.inh.low
med.high <- inh.packets * c.inh.high

params[p == "cmed9H", mid := med.mid] 
params[p == "cmed9H", low := med.low] 
params[p == "cmed9H", high := med.high] 

med.cost.9H <- params[p == "cmed9H", mid]

appt <- appt.num.9H * c.gp.review + c.liver 

spec.appt <- appt.num.9H * c.spec.review + c.liver

params[p == "ctreat9H", mid := appt + med.cost.9H] 

params[p == "cparttreat9H",
       mid := appt / part.appt + med.cost.9H / part.med] 

params[p == "ctreatspec9H", mid := spec.appt + med.cost.9H] 

params[p == "cparttreatspec9H",
       mid := spec.appt / part.appt + med.cost.9H / part.med] 

# Cost of active TB
params[p == "ctb", mid := 19212.89] 
params[p == "ctb", low := 12405.78] 
params[p == "ctb", high := 30649.78] #18491.84

# Cost of sae
params[p == "csae", mid := 1222.64] 
params[p == "csae", low := 500] 
params[p == "csae", high := 10000] 

params[p == "attscreen", mid := 0.325]
params[p == "attscreen", low := 0.2]
params[p == "attscreen", high := 0.6]

params[p == "att", mid := 0.8748806]
params[p == "att", low := 0.8191342]
params[p == "att", high := 0.9334222]

params[p == "begintrt", mid := 0.596]
params[p == "begintrt", low := 0.262]
params[p == "begintrt", high := 0.762]

params[p == "snqftgit", mid := 0.6538] # 0.6104
params[p == "snqftgit", low := 0.4438] # 0.4925
params[p == "snqftgit", high := 0.8279] # 0.7195

params[p == "spqftgit", mid := 0.96942] # 0.95820
params[p == "spqftgit", low := 0.953365] # 0.95700
params[p == "spqftgit", high := 0.984865] # 0.95948

params[p == "sntst15", mid := 0.6923] # 0.6753
params[p == "sntst15", low := 0.4821] # 0.5590
params[p == "sntst15", high := 0.8567] # 0.7777

params[p == "sptst15", mid := 0.99910] # 0.95117
params[p == "sptst15", low := 0.983685] # 0.94978
params[p == "sptst15", high := 1] # 0.95255

params[p == "sntst10", mid := 0.7692] # 0.7532
params[p == "sntst10", low := 0.5635] # 0.6418
params[p == "sntst10", high := 0.9103] # 0.8444

params[p == "sptst10", mid := 0.89119] # 0.82227
params[p == "sptst10", low := 0.873993] # 0.81780
params[p == "sptst10", high := 0.907993] # 0.82686

params[p == "treat.effic.3HP", mid := 0.69] # IUAT
params[p == "treat.effic.3HP", low := 0.28] # MMWR Guidelines for LTBI treatment 2020 2018 Zenner update
params[p == "treat.effic.3HP", high := 0.82] # Zenner
params[p == "treat.complete.3HP", mid := 0.764] # Belknap
params[p == "treat.complete.3HP", low := 0.740] # Belknap
params[p == "treat.complete.3HP", high := 0.900] # Denholm

params[p == "treat.effic.4R", mid := 0.69] # IUAT
params[p == "treat.effic.4R", low := 0.50] # MMWR Guidelines for LTBI treatment 2020 2018 Zenner update
params[p == "treat.effic.4R", high := 0.88] # MMWR Guidelines for LTBI treatment 2020 2018 Zenner update
params[p == "treat.complete.4R", mid := 0.852] # Belknap
params[p == "treat.complete.4R", low := 0.535] # Roland
params[p == "treat.complete.4R", high := 0.872] # Denholm

params[p == "treat.effic.6H", mid := 0.69] # IUAT
params[p == "treat.effic.6H", low := 0.41] # MMWR Guidelines for LTBI treatment 2020 2018 Zenner update
params[p == "treat.effic.6H", high := 0.74] # Zenner
params[p == "treat.complete.6H", mid := 0.78] # Belknap
params[p == "treat.complete.6H", low := 0.441] # Belknap
params[p == "treat.complete.6H", high := 0.85] # Denholm

params[p == "treat.effic.9H", mid := 0.69] # IUAT
params[p == "treat.effic.9H", low := 0.41] # MMWR Guidelines for LTBI treatment 2020 2018 Zenner update
params[p == "treat.effic.9H", high := 0.93] # Zenner
params[p == "treat.complete.9H", mid := 0.73] # Belknap
params[p == "treat.complete.9H", low := 0.4] # Belknap
params[p == "treat.complete.9H", high := 0.85] # Denholm

params[p == "ttt3HP", mid := 0.292]
params[p == "ttt3HP", low := 0.028]
params[p == "ttt3HP", high := 0.417]

params[p == "ttt4R", mid := 0.333]
params[p == "ttt4R", low := 0.250]
params[p == "ttt4R", high := 0.458]

params[p == "ttt6H", mid := 0.417]
params[p == "ttt6H", low := 0.333]
params[p == "ttt6H", high := 0.542]

params[p == "ttt9H", mid := 0.542]
params[p == "ttt9H", low := 0.458]
params[p == "ttt9H", high := 0.667]

# Utility calculations
# the healthy state utility remains constant

uhealthy.fix <- 0.8733333

params[p == "uhealthy", mid := uhealthy.fix]
params[p == "uhealthy", low := uhealthy.fix]
params[p == "uhealthy", high := uhealthy.fix]

healthy.base <- 0.07166667
healthy.1mth <- 0.07333333
healthy.2mths <- 0.07166667
healthy.4mths <- 0.07333333
healthy.6mths <- 0.07250000
healthy.9mths <- 0.07250000
healthy.12mths <- 0.07416667

ultbi.base <- 0.07000000
ultbi.1mth<- 0.06833333
ultbi.2mths <- 0.06833333
ultbi.4mths <- 0.06916667
ultbi.6mths <- 0.06833333
ultbi.9mths <- 0.06833333
ultbi.12mths <- 0.07000000

utb.base <- 0.06000000
utb.1mth <- 0.06666667
utb.2mths <- 0.07083333
utb.4mths <- 0.07083333
utb.6mths <- 0.07083333
utb.9mths <- 0.07583333
utb.12mths <- 0.07583333

# Active TB utility calculations
sae.decrement <- 0.25

uactivetbfunct <- function(symptom.mths, sae.mths, chance.of.sae) {
  if (symptom.mths == 6) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      2 * utb.4mths + 1 * utb.6mths
  } else if (symptom.mths == 4) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      2 * utb.4mths + 3 * utb.6mths
  } else if (symptom.mths == 3) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      2 * utb.4mths + 3 * utb.6mths + 1 * utb.9mths
  } else if (symptom.mths == 2) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      2 * utb.4mths + 3 * utb.6mths + 2 * utb.9mths
  } else if (symptom.mths == 1) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      2 * utb.4mths + 3 * utb.6mths + 3 * utb.9mths
  } else if (symptom.mths == 8) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 2 * utb.2mths +
      1 * utb.4mths
  } else if (symptom.mths == 10) {
    utbactive <- (utb.base * symptom.mths) + utb.1mth + 1 * utb.2mths
  }
  utbactive.sae <- ((utbactive/12) * (12 - sae.mths)) + 
    (sae.mths * ((uhealthy.fix - sae.decrement)/12))
  utbactive <- (utbactive.sae * chance.of.sae) + 
    ((1 - chance.of.sae) * utbactive)
  utbactive
}


# uhealthy.fix - uactivetbfunct(8, 2, 0.0051)
# # 0.1142495
# uhealthy.fix - uactivetbfunct(4, 0.5, 0.003)
# # 0.0708244


params[p == "uactivetb", mid := uactivetbfunct(3, 1, 0.007)]
params[p == "uactivetb", low := 0.7068]
params[p == "uactivetb", high := uactivetbfunct(1, 0.5, 0.003)]

# LTBI treatment utility calculations

part.utility.dec <- 0.5


ultbi3HPcalc <- ultbi.base + ultbi.1mth + ultbi.2mths +
  2 * healthy.4mths + 3 * healthy.6mths + 2 * healthy.9mths + 2 * healthy.12mths

ultbi4Rcalc <- ultbi.base + ultbi.1mth + 2 * ultbi.2mths + 
  2 * healthy.4mths + 2 * healthy.6mths + 2 * healthy.9mths + 2 * healthy.12mths

ultbi6Hcalc <- ultbi.base + ultbi.1mth + 2 * ultbi.2mths + 2 * ultbi.4mths +
  2 * healthy.6mths + 2 * healthy.9mths + 2 * healthy.12mths

ultbi9Hcalc <- ultbi.base + ultbi.1mth + 2 * ultbi.2mths + 
  2 * ultbi.4mths + 2 * ultbi.6mths + 2 * ultbi.9mths + 2 * ultbi.12mths


params[p == "ultbi3HP", mid := uhealthy.fix]
params[p == "ultbi3HP", low := ultbi3HPcalc]
params[p == "ultbi3HP", high := uhealthy.fix]
params[p == "ultbi3HP", distribution := "uniform"]
params[p == "ultbipart3HP", mid := uhealthy.fix]
params[p == "ultbipart3HP", low := uhealthy.fix -
         ((uhealthy.fix - ultbi3HPcalc) * part.utility.dec)]
params[p == "ultbipart3HP", high := uhealthy.fix]
params[p == "ultbipart3HP", distribution := "uniform"]

params[p == "ultbi4R", mid := uhealthy.fix]
params[p == "ultbi4R", low := ultbi4Rcalc]
params[p == "ultbi4R", high := uhealthy.fix]
params[p == "ultbi4R", distribution := "uniform"]
params[p == "ultbipart4R", mid := uhealthy.fix]
params[p == "ultbipart4R", low := uhealthy.fix -
         ((uhealthy.fix - ultbi4Rcalc) * part.utility.dec)]
params[p == "ultbipart4R", high := uhealthy.fix]
params[p == "ultbipart4R", distribution := "uniform"]

params[p == "ultbi6H", mid := uhealthy.fix]
params[p == "ultbi6H", low := ultbi6Hcalc]
params[p == "ultbi6H", high := uhealthy.fix]
params[p == "ultbi6H", distribution := "uniform"]
params[p == "ultbipart6H", mid := uhealthy.fix]
params[p == "ultbipart6H", low := uhealthy.fix -
         ((uhealthy.fix - ultbi6Hcalc) * part.utility.dec)]
params[p == "ultbipart6H", high := uhealthy.fix]
params[p == "ultbipart6H", distribution := "uniform"]

params[p == "ultbi9H", mid := uhealthy.fix]
params[p == "ultbi9H", low := ultbi9Hcalc]
params[p == "ultbi9H", high := uhealthy.fix]
params[p == "ultbi9H", distribution := "uniform"]
params[p == "ultbipart9H", mid := uhealthy.fix]
params[p == "ultbipart9H", low := uhealthy.fix -
         ((uhealthy.fix - ultbi9Hcalc) * part.utility.dec)]
params[p == "ultbipart9H", high := uhealthy.fix]
params[p == "ultbipart9H", distribution := "uniform"]

params[p == "uactivetbr", mid := 0.873333333]
params[p == "uactivetbr", low := 0.849333333]
params[p == "uactivetbr", high := 0.873333333]

params[p == "ultbitreatsae", mid := 0.8525000]
params[p == "ultbitreatsae", low := 0.8420833]
params[p == "ultbitreatsae", high := 0.8629167]

# Write the table to clipboard so I can 
# paste it into my Excel spreadsheet
write.table(params, file = "clipboard-16384", 
            sep = "\t", row.names = FALSE)

# Save this table to file
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
saveRDS(params, "params onshore.rds")









