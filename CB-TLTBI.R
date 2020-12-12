

# MODEL RUN SCRIPT
# This is the script that is called on to set up and run the model 
# by all other scripts, except the PSA.
# It defines all the states and transition matrices
# The strategies and all parameters are defined in the "Parameter values" document.

# Coding style
# https://google.github.io/styleguide/Rguide.xml

# library(RevoScaleR)
library(lazyeval) # required
library(data.table) # required


setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
source("CB-TLTBI Functions.R")

if (exists("parameters.already.set") == TRUE) {
  
} else {
  source("Parameter values.R")
}

source("CB-TLTBI_DataPreparation.R")

# Read the data files (if required)
vic.mortality <- readRDS("Data/aust.mortality.rds") # this is also required
vic.mortality <- as.data.table(vic.mortality)
    # Projected mortality rates for Australia from:
    # https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3222.02017%20(base)%20-%202066?OpenDocument
    # Results then aggregated by gender assuming a gender weighting by age equivalent to the Australian population
    # in September 2018. Source: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Sep%202018?OpenDocument
vic.tb.mortality <- readRDS("Data/tb.mortality.rds")
vic.tb.mortality <- as.data.table(vic.tb.mortality)
vic.tb.mortality[, age := as.integer(age)]
    # TB mortality data from: Dale K, Tay E, Trevan P, et al. Mortality among tuberculosis cases 
    # in Victoria, 2002-2013: case fatality and factors associated with death. 
    # Int J Tuberc Lung Dis 2016;20(4):515-23. doi: 10.5588/ijtld.15.0659
sae.rate <- readRDS("Data/sae.rate.rds") # this is also required
sae.rate <- as.data.table(sae.rate)

    # SAE rate from: several sources, e.g. Campbell et al 2019
sae.mortality <- readRDS("Data/sae.mortality.rds") # this is also required
sae.mortality <- as.data.table(sae.mortality)
    # SAE mortality data from: ...
rradjrates <- readRDS("Data/rradjrates.rds")

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

# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QFTGIT", "TST10", "TST15"), 
                       SN = c(snqftgit, sntst10, sntst15),
                       SP = c(spqftgit, sptst10, sptst15),
                       # Sensitivity and specificity values from: Abubakar I, Drobniewski F, Southern J, et al. Prognostic value 
                       # of interferon-gamma release assays and tuberculin skin test in predicting the development of active 
                       # tuberculosis (UK PREDICT TB): a prospective cohort study. Lancet Infect Dis 2018; 18(10): 1077-87.
                       # cost.primary = c(74.34, 70.40, 70.40))
                       cost.primary = c(cscreenqft, cscreentst, cscreentst))

# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("3HP","4R", "6H", "9H"),
                           treat.complete = c(treat.complete.3HP, treat.complete.4R, treat.complete.6H, treat.complete.9H),
                           treat.effic = c(treat.effic.3HP, treat.effic.4R, treat.effic.6H, treat.effic.9H))

# Create a sample treatment cost data table
treatmentcost.dt <- data.table(treatment = c("3HP","4R", "6H", "9H", "3HP","4R", "6H", "9H"),
                               practitioner = c("gp","gp", "gp", "gp", 
                                                "spec","spec", "spec", "spec"),
                               cost.primary = c(ctreat3HP, ctreat4R, ctreat6H, ctreat9H,
                                                ctreatspec3HP, ctreatspec4R, ctreatspec6H, 
                                                ctreatspec9H),
                               cost.partial = c(cparttreat3HP, cparttreat4R,
                                                cparttreat6H, cparttreat9H,
                                                cparttreatspec3HP, cparttreatspec4R,
                                                cparttreatspec6H, cparttreatspec9H),
                               cost.sae = c(csae3HP, csae4R,
                                            csae6H, csae9H,
                                            csae3HP, csae4R,
                                            csae6H, csae9H))

# This data table indicates when those who receive LTBI treatment in the first 
# year after migration are likely to have received that treatment (as an annual proportion).
timetotreat.dt <- data.table(treatment = c("3HP", "4R", "6H", "9H"),
                           yearfraction = c(ttt3HP, ttt4R, ttt6H, ttt9H))

# Create a sample utility data table
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
                                                 # (ultbipart4R/(1 - TREATR4R - SAE4R)) * (treat.complete.1 + treat.complete.2), 
                                                 # (ultbi4R/TREATR4R) * treat.complete.4R,
                                                  ultbitreatsae, 0,
                                                  uhealthy,
                                                  uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
                                                 # (ultbipart4R/(1 - TREATR4R - SAE4R)) * (treat.complete.1 + treat.complete.2), 
                                                 # (ultbi4R/TREATR4R) * treat.complete.4R,
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

unevaluated.flow.cost <- lazy(c(0, 0, param$TESTC, 
                                param$TESTC + param$ATTENDCOST, param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 
                                param$TESTC + param$ATTENDCOST + param$TREATC + param$SAECOST,
                                #param$TESTC + param$ATTENDCOST + param$TREATC + (((param$TREATCOMPLETE - param$TREATR) * (1/param$TREATR)) * (param$TREATC - param$PARTIALTREATCOST) ),
                                param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 0,
                                0,
                                0, 0, param$TESTC, param$TESTC + param$ATTENDCOST, 
                                param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 
                                #param$TESTC + param$ATTENDCOST + param$TREATC + (((param$TREATCOMPLETE - param$TREATR) * (1/param$TREATR)) * (param$TREATC - param$PARTIALTREATCOST) ),
                                param$TESTC + param$ATTENDCOST + param$TREATC + param$SAECOST,
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

# # BASELINE.S1.TM
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


# # S1.TM
# # manually create list of values ()
# list.values <- c(0,	quote(1 - (param$POP * param$ATTENDSCREEN)),	quote((param$POP * param$ATTENDSCREEN) * (1 - (1-param$TESTSP) * param$ATTEND)),	quote((param$POP * param$ATTENDSCREEN) * (1 - param$TESTSP) * param$ATTEND * (1 - param$BEGINTREAT)),	quote((param$POP * param$ATTENDSCREEN) * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * (1 - param$TREATCOMPLETE - param$SAE)),	quote((param$POP * param$ATTENDSCREEN) * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$TREATCOMPLETE),	quote((param$POP * param$ATTENDSCREEN) * (1 - param$TESTSP) * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	quote(param$SAEMR),	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote((1 - (param$POP * param$ATTENDSCREEN)) * (1 - (param$RR * param$RRADJUST))),	quote((param$POP * param$ATTENDSCREEN) * (1 - (param$TESTSN * param$ATTEND) - (param$RR * param$RRADJUST * (1 - ((param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT)))))),	quote(param$POP * param$ATTENDSCREEN * param$TESTSN * param$ATTEND * (1 - param$BEGINTREAT)),	quote(param$POP * param$ATTENDSCREEN * param$TESTSN * param$ATTEND * param$BEGINTREAT * (1 - param$TREATCOMPLETE - param$SAE)),	quote(param$POP * param$ATTENDSCREEN * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATCOMPLETE),	quote((param$POP * param$ATTENDSCREEN) * param$TESTSN * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	0,	0,	quote(((1 - (param$POP * param$ATTENDSCREEN)) * param$RR * param$RRADJUST) + ((param$POP * param$ATTENDSCREEN) * param$RR * param$RRADJUST * (1 - ((param$TESTSN * param$ATTEND * param$BEGINTREAT * param$TREATR) * (1 - param$TIMETOTREAT))))),	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$PART.TREAT.EFFICACY),	quote((1-param$PART.TREAT.EFFICACY)*param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$FULL.TREAT.EFFICACY - (param$MR*param$FULL.TREAT.EFFICACY)),	quote(((1- param$FULL.TREAT.EFFICACY) - (param$MR*(1-param$FULL.TREAT.EFFICACY))) *param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
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
# #saveRDS(S1.TMKD,file = "Data/S1.TMKD.rds")
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
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "Med"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               # TBMR = 0.001,
                               RRADJUST = Get.RRADJ(DT, year),
                               # RRADJUST takes into account the fact that a proportion (~12% in Victoria)
                               # of TB cases are picked up each year with existing TB control strategies, i.e.
                               # during follow-up as a result of an abnormal CXR during pre-migration off-shore screening.
                               BEGINTREAT = begintrt,
                               ATTEND = att,
                               # Proportion of migrants referred following off-shore screening (CXR) 
                               # that attend follow-up appointment once onshore. 
                               # Source: Flynn MG, Brown LK. Treatment of latent tuberculosis in migrants 
                               # to Victoria. Commun Dis Intell Q Rep 2015; 39(4): E578-83.
                               ATTENDSCREEN = attscreen,
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
                               TREATR = Get.TREATR(C = "treat.complete", E = "treat.effic", treatment),
                               TREATCOMPLETE = Get.TREAT(S = "treat.complete", treatment),
                               PART.TREAT.EFFICACY = Get.PART.EFFIC(DT, C = "treat.complete", E = "treat.effic", treatment),
                               FULL.TREAT.EFFICACY = Get.FULL.EFFIC(DT, E = "treat.effic", treatment),
                               TREATC = Get.TREATC(S = "cost.primary", treatment),
                               POP = Get.POP(DT, strategy),
                               UTILITY = Get.UTILITY(treatment),
                               #ATTENDCOST = Get.ATTENDC(DT, S = "cost.attend"),
                               ATTENDCOST = cattend,
                               PARTIALTREATCOST = Get.TREATC(S = "cost.partial", treatment),
                               TBCOST = ctb,
                               SAECOST = Get.TREATC(S = "cost.sae", treatment)
                               )
 
# Uses aust.rds file to create a sample input
pop.master <- CreatePopulationMaster()
# pop.master <- subset(pop.master, AGERP == 60 & ISO3 == "40-99")
# pop.master <- subset(pop.master, AGERP == 20 & ISO3 == "200+")

# factor <-  234.404422824184 #      71 * 0.74 * 0.595 * 0.84 * 0.61 * 0.76 * 0.53 * 1.29 * 0.77 * 0.84 * 1.96 * 0.67 * 0.83
# 
# pop.master[, NUMP := NUMP/factor]
# pop.master[, LTBP := LTBP/factor]
# pop.master[, p.sus := p.sus/factor]
# pop.master[, p.ltbi := p.ltbi/factor]

#set.seed(10)
#pop.master <- pop.master[sample(.N, 200)]

# age.limit.older.than <- 19
# age.limit.younger.than <- 30
# target.tbincid <- "100+"

# POP.SUBSET.FUNCTION <- function(DT, strategy) {
# 
#   if (target.tbincid == "100+") {
#     DT <- subset(DT, ISO3 == "100-149")
#     DT <- subset(DT, AGERP > age.limit.older.than & AGERP < age.limit.younger.than)
#   } else if (target.tbincid == "<100") {
#     DT <- subset(DT, ISO3 == "40-99")
#     DT <- subset(DT, AGERP > age.limit.older.than & AGERP < age.limit.younger.than)
#   }
#   DT
# }
#
# pop.master[, LTBP := (LTBP/NUMP) * 10]
# pop.master[, NUMP := 10]
# pop.master[, p.ltbi := LTBP]
# pop.master[, p.sus := NUMP - LTBP]

# pop.master[, LTBP := (LTBP/NUMP) * 1000]
# pop.master[, NUMP := 1000]
# pop.master[, p.ltbi := LTBP]
# pop.master[, p.sus := NUMP - LTBP]


#---------- Model parameters for STRATEGY 0 ----------------#

PSA <- 0
discount <- disc
start.year <- startyear
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- totalcycles  # The mortality data continues until 2100 and migrant inflows are
              # possible until 2050

#---------------Baseline for S1 --------------------#
DoRunModel(S0_12, start.year, cycles)

#-------- Model parameters for S1 & S2 --------#

# discount <- disc
# start.year <- startyear
# year <- start.year # Initialise year with start.year
# markov.cycle <- 0 # Tracks the current cycle
# cycles <- totalcycles  # The mortality data continues until 2100 and migrant inflows are
#               # possible until 2050



DoRunModel(S2, start.year, cycles)