# Coding style
# https://google.github.io/styleguide/Rguide.xml


# Load libraries. (not needed if using the *.rds data files objects)
#library(tidyverse)
#library(reshape2)
#library(zoo) # used for filling empty AGEP values
#library(readxl)
#library(ggplot2)
#library(rlang)
#library(diagram)
#library(heemod)

# library(RevoScaleR)
library(lazyeval) # required
library(data.table) # required



# Model setup located within this file.
# It defines all the states, transition matrices, strategies, costs and parameters.
source("CB-TLTBI_DataPreparation.R")
source("CB-TLTBI Functions.R")


# This function uses the above three Fix* functions. 
# Run once to create the *.rds objects (vic.fertility, vic.mortality, vic.migration)
# based on ABS's population projection data
# CreateRDSDataFiles()

# Read the data files (if required)

# aust <- readRDS("Data/aust.rds")
#aust.vic <- readRDS("Data/aust.vic.rds") # this is required for S1,S2,S3,S4 and S5
aust.vic <- readRDS("Data/Aust16byTBincid.rds") 
    # Australian 2016 census data extracted from Table Builder by country of birth
    # (place of usual residence), single year age and single year of arrival. 
# aust.vic.LGA <- readRDS("Data/aust.vic.LGA.rds") # this is for S0
# prob.Inf <- readRDS("Data/prob.Inf.rds") 
# tbhaz.200rep <- readRDS("Data/tbhaz.200rep.rds")
# tbhaz.5000rep <- readRDS("Data/tbhaz.5000rep.rds")
# vic.fertility <- readRDS("Data/vic.fertility.rds")
#vic.mortality <- readRDS("Data/vic.mortality.rds") # this is also required
vic.mortality <- readRDS("Data/aust.mortality.rds") # this is also required
vic.mortality <- as.data.table(vic.mortality)
    # Projected mortality rates for Australia from:
    # https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3222.02017%20(base)%20-%202066?OpenDocument
    # Results then aggregated by gender assuming a gender weighting by age equivalent to the Australian population
    # in September 2018. Source: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Sep%202018?OpenDocument
# vic.migration <- readRDS("Data/vic.migration.rds")
# vic.pop <- readRDS("Data/vic.pop.rds")
#RRates <- readRDS("Data/RRates.rds") # this is also required
RRates <- readRDS("Data/RRatescobincidnosex.rds") # this is also required
    # TB reactivation rate data from: Dale K, Trauer J, et al. Estimating long-term tuberculosis 
    # reactivation rates in Australian migrants. Clinical Infectious Diseases 2019 (in press)
#vic.tb.mortality <- readRDS("Data/vic.tb.mortality.rds") # this is also required
vic.tb.mortality <- readRDS("Data/tb.mortality.rds") # this is also required
vic.tb.mortality <- as.data.table(vic.tb.mortality)
    # TB mortality data from: Dale K, Tay E, Trevan P, et al. Mortality among tuberculosis cases 
    # in Victoria, 2002-2013: case fatality and factors associated with death. 
    # Int J Tuberc Lung Dis 2016;20(4):515-23. doi: 10.5588/ijtld.15.0659
# begintreat.rate <- readRDS("Data/begintreat.rate.rds") # this is also required
# begintreat.rate <- as.data.table(begintreat.rate)
    # ??? data from: ...
sae.rate <- readRDS("Data/sae.rate.rds") # this is also required
sae.rate <- as.data.table(sae.rate)
    # SAE rate from: ...
sae.mortality <- readRDS("Data/sae.mortality.rds") # this is also required
sae.mortality <- as.data.table(sae.mortality)
    # SAE mortality data from: ...
emigrate.rate <- readRDS("Data/emigrate.rate.rds") # this is also required
emigrate.rate <- as.data.table(emigrate.rate)
    # emigrate data from: ...



# Creating a vector of state names
# state.names <- c("p.sus", "p.sus.fp.t", "p.sus.fp.nt", "p.sus.fp.tc", "p.sus.tn",
#                  "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.nt", "p.ltbi.tp.nt.tb",
#                  "p.ltbi.tp.nt.tbr", "p.ltbi.fn", "p.ltbi.fn.tb", "p.ltbi.fn.tbr", "p.ltbi.tb",
#                  "p.ltbi.tbr", "p.ltbi.tp.nt.tb.death", "p.ltbi.fn.tb.death", "p.ltbi.tb.death", "p.death")
# Creating a vector of state names - KD
state.names <- c("p.sus",	"p.sus.nf",	"p.sus.nbt",	"p.sus.nct",	"p.sus.tc",
                 "p.sus.sae",	"p.sus.sae.death",
                 "p.ltbi",	"p.ltbi.nf",	"p.ltbi.nbt",	"p.ltbi.nct",	"p.ltbi.tc",
                 "p.ltbi.sae",	"p.ltbi.sae.death",
                 "p.tb",	"p.tbr",	"p.tb.death",	"p.death",	"p.emigrate")


# Number of states
state.number <- length(state.names)

# a hack to manage the flows, state.cost, flow.cost and state.qaly values.
new.state.names <- c(state.names, paste("V.", state.names, sep = ""),
                     paste("SC.", state.names, sep = ""),
                     paste("FC.", state.names, sep = ""),
                     paste("SQ.", state.names, sep = ""))


# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QTFGIT", "TST05", "TST10", "TST15"), SN = c(0.6104, 0.74, 0.7532, 0.6753),
                       SP = c(0.7784, 0.56, 0.6679, 0.7726), 
                       # Sensitivity and specificity values from: Abubakar I, Drobniewski F, Southern J, et al. Prognostic value 
                       # of interferon-gamma release assays and tuberculin skin test in predicting the development of active 
                       # tuberculosis (UK PREDICT TB): a prospective cohort study. Lancet Infect Dis 2018; 18(10): 1077-87.
                       cost.primary = c(79.75, 67.10, 67.10, 67.10),
                       cost.tertiary = c(122.71, 164.5, 164.5, 164.5))


# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           rate = c(.83, .78, .82, .63),
                           cost.primary = c(437.13, 578.87, 440.34, 436.42),
                           cost.tertiary = c(632.38, 969.37, 596.54, 709.77),
                           sae = c(0.000000009, 0.00000025, 0.00000016, 0.0000002))

# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           rate = c(.83, .78, .82, .63),
                           cost.primary = c(437.13, 578.87, 440.34, 436.42),
                           cost.tertiary = c(632.38, 969.37, 596.54, 709.77))

#9H cost changed from 549.22 to 578.87 and 939.72 to 969.37 respectively. 

# Create a sample data table to give the reactivation rate reduction in the treatment year
treatmentyearRR.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           ratereduction = c(0.4, 0.8, 0.3, 0.6))
# need to talk to Michael Flynn to establish how long it takes to complete treatment

# Create a sample utility data table
# TODO: fix hard coded data table. It should take state.names and create the columns.
utility.dt <- data.table(treatment = c("", "4R", "9H", "3HP", "6H"))
utility.dt[, c(state.names) := as.numeric(NA)]

# Utility values
uhealthy <- 0.870
# Bauer et al 2015
uactivetb <- 0.803
uactivetbr <- 1
# Bauer et al 2015
ultbi4R <- 0.823
# Bauer et al 2015
ultbi9H <- 0.823
# Bauer et al 2015
ultbi3HP <- 0.823
ultbi6H <- 0.823
ultbitreatsae <- 0.75 #to do


utility.dt[treatment == "6H", c(state.names) := .(uhealthy, ultbi6H, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uhealthy, ultbi6H, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uactivetb, uactivetbr, 0, 0, 0)]

utility.dt[treatment == "9H", c(state.names) := .(uhealthy, ultbi9H, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uhealthy, ultbi9H, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uactivetb, uactivetbr, 0, 0, 0)]

utility.dt[treatment == "4R", c(state.names) := .(uhealthy, ultbi4R, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uhealthy, ultbi4R, uhealthy, uhealthy, uhealthy,
                                                  ultbitreatsae, 0,
                                                  uactivetb, uactivetbr, 0, 0, 0)]

utility.dt[treatment == "3HP", c(state.names) := .(uhealthy, ultbi3HP, uhealthy, uhealthy, uhealthy,
                                                   ultbitreatsae, 0,
                                                   uhealthy, ultbi3HP, uhealthy, uhealthy, uhealthy,
                                                   ultbitreatsae, 0,
                                                   uactivetb, uactivetbr, 0, 0, 0)]

utility.dt[treatment == "", c(state.names) := .(uhealthy, NA, NA, NA, NA,
                                                NA, NA,
                                                uhealthy, NA, NA, NA, NA,
                                                NA, NA,
                                                uactivetb, uactivetbr, 0, 0, 0)]


state.names <- c("p.sus",	"p.sus.nf",	"p.sus.nbt",	"p.sus.nct",	"p.sus.tc",
                 "p.sus.sae",	"p.sus.sae.death",
                 "p.ltbi",	"p.ltbi.nf",	"p.ltbi.nbt",	"p.ltbi.nct",	"p.ltbi.tc",
                 "p.ltbi.sae",	"p.ltbi.sae.death",
                 "p.tb",	"p.tbr",	"p.tb.death",	"p.death",	"p.emigrate")

# FC prefix
unevaluated.flow.cost <- lazy(c(0, 0, param$ATTENDCOST, param$PARTIALTREATCOST, param$TREATC,
                                param$SAECOST, 0,
                                0, 0, param$ATTENDCOST, param$PARTIALTREATCOST, param$TREATC,
                                param$SAECOST, 0,
                                0, 0, 0, 0, 0))

# SC prefix
unevaluated.state.cost <- lazy(c(0, 0, 0, 0, 0,
                                 0, 0,
                                 0, 0, 0, 0, 0,
                                 0, 0,
                                 param$TBCOST, 0, 0, 0, 0))

# SQ prefix
unevaluated.state.utility <- lazy(c(0, 0, 0, 0, 0,
                                    0, 0,
                                    0, 0, 0, 0, 0,
                                    0, 0,
                                    param$TBCOST, 0, 0, 0, 0))

#Sample commands demonstrating the functional argument list.


arglist <- CreateArgumentList(state.names, state.number)

# updates a row. Note: unevaluated parameter must be wrapped in a quote()
# arglist$update.row(9, c(0, 0, 0, 0, 0, 0, 0, 0, 0, quote(CMP), 0, 0, 0, 0, 0, 0, 0, 0, quote(param$TBMR), 0, 0, 0, quote(param$MR)))
# arglist$update.list(listvalues) # For passing a entire list
# arglist$update.cell(7, 20, quote(param$MR+param$TREATSAE)) # update on cell


# Show list with N x N state dimensions
# arglist$show.list()[2,20]

# Add the state names as the final argument
# arglist$add.state.name(state.names)

# Drop the state name and reset the dimension.
# arglist$drop.state.name()

# Save the argument list. 
# arglist$save.list("BASELINE.S1.TM")


# BASELINE.S1.TM
# manually create list of values ()
# list.values <- c(0,	quote(param$POP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	quote(param$POP * param$RR * param$RRADJUST),	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	quote(param$RR * param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$TBMR),	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1)
# arglist$update.list(list.values) # For passing a entire list
# arglist$add.state.name(state.names)
# # saveRDS(S1.TMKD,file = "Data/BASELINE.S1.TM.rds")
# arglist$save.list("BASELINE.S1.TMKD")

# 
# S1.TM
# manually create list of values ()
# list.values <- c(0,	quote(CMP),	quote((param$POP * (1 - param$TSTSP) * param$ATTEND) * (1 - param$BEGINTREAT)),	quote((param$POP * (1 - param$TSTSP) * param$ATTEND) * param$BEGINTREAT * (1 - param$TREATR - param$SAE) ),	quote(param$POP * (1 - param$TSTSP) * param$ATTEND * param$BEGINTREAT * param$TREATR),	quote(param$POP * (1 - param$TSTSP) * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	quote(CMP),	quote(param$SAEMR),	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote((param$POP - (param$POP * param$RR * param$RRADJUST * ((param$POP - (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR))/param$POP))) * param$TSTSN * param$ATTEND * (1 - param$BEGINTREAT)),	quote((param$POP - (param$POP * param$RR * param$RRADJUST * ((param$POP - (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR))/param$POP))) * param$TSTSN * param$ATTEND * param$BEGINTREAT * (1 - param$TREATR - param$SAE)),	quote((param$POP - (param$POP * param$RR * param$RRADJUST * ((param$POP - (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR))/param$POP))) * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR),	quote((param$POP - (param$POP * param$RR * param$RRADJUST * ((param$POP - (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR))/param$POP))) * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$SAE),	0,	quote(param$POP * param$RR * param$RRADJUST * ((param$POP - (param$POP * param$TSTSN * param$ATTEND * param$BEGINTREAT * param$TREATR))/param$POP)),	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	quote(param$RR*param$RRADJUST),	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	0,	0,	0,	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$SAEMR),	quote(param$RR*param$RRADJUST),	0,	0,	0,	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	quote(param$TBMR),	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	quote(CMP),	0,	quote(param$MR),	quote(param$EMIGRATE),
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,
#                  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1)
# arglist$update.list(list.values)
# arglist$add.state.name(state.names)
# # saveRDS(S1.TMKD,file = "Data/S1.TMKD.rds")
# arglist$save.list("S1.TMKD")

# # BASELINE.S1.TM
# arglist$update.list(listvalues) # For passing a entire list
# arglist$add.state.name(state.names)
# arglist$save.list("BASELINE.S1.TMKD")

# Load the argument list
# S1.TM
# S2.TM
# BASELINE.TM

# arglist.S1.TM <- arglist$load.list("S1.TM")
arglist.S1.TM <- arglist$load.list("S1.TMKD")
# arglist.S2.TM <- arglist$load.list("S2.TM")
# arglist.BASELINE.TM <- arglist$load.list("BASELINE.TM")
# arglist.BASELINE.S1.TM <- arglist$load.list("BASELINE.S1.TM")
arglist.BASELINE.S1.TM <- arglist$load.list("BASELINE.S1.TMKD")

CreateStates(state.names) # instantiates a set of states objects with default values

# Create a set of strategies
# S1 <- DefineStrategy(p.sus, p.sus.fp, p.sus.fp.a, p.sus.fp.t, p.sus.fp.t.sae,
#                      p.sus.fp.sae.death, p.sus.fp.tc, p.sus.nt,
#                      p.ltbi, p.ltbi.tp, p.ltbi.tp.a, p.ltbi.tp.t, p.ltbi.tp.t.sae,
#                      p.ltbi.tp.sae.death, p.ltbi.tp.tc, p.ltbi.nt,
#                      p.tb, p.tbr, p.tb.death, p.death, p.emigrate,
#                      transition.matrix = do.call(DefineTransition, arglist.S1.TM))
S1 <- DefineStrategy(p.sus,	p.sus.nf,	p.sus.nbt,	p.sus.nct,	p.sus.tc,
                     p.sus.sae,	p.sus.sae.death,
                     p.ltbi,	p.ltbi.nf,	p.ltbi.nbt,	p.ltbi.nct,	p.ltbi.tc,	
                     p.ltbi.sae,	p.ltbi.sae.death,
                     p.tb,	p.tbr,	p.tb.death,	p.death,	p.emigrate,
                     transition.matrix = do.call(DefineTransition, arglist.S1.TM))

# S2 <- DefineStrategy(p.sus, p.sus.fp, p.sus.fp.a, p.sus.fp.t, p.sus.fp.t.sae,
#                      p.sus.fp.sae.death, p.sus.fp.tc, p.sus.nt,
#                      p.ltbi, p.ltbi.tp, p.ltbi.tp.a, p.ltbi.tp.t, p.ltbi.tp.t.sae,
#                      p.ltbi.tp.sae.death, p.ltbi.tp.tc, p.ltbi.nt,
#                      p.tb, p.tbr, p.tb.death, p.death, p.emigrate,
#                      transition.matrix = do.call(DefineTransition, arglist.S2.TM))

# The same transition matrix is used for scenario 3(5%) , 4(10%). The object name triggers the Get.POP function
# to return the testing percentage.

# S3 <- S2
# S4 <- S2
# S5 <- S2

# S0_12 <- DefineStrategy(p.sus, p.sus.fp, p.sus.fp.a, p.sus.fp.t, p.sus.fp.t.sae,
#                         p.sus.fp.sae.death, p.sus.fp.tc, p.sus.nt,
#                         p.ltbi, p.ltbi.tp, p.ltbi.tp.a, p.ltbi.tp.t, p.ltbi.tp.t.sae,
#                         p.ltbi.tp.sae.death, p.ltbi.tp.tc, p.ltbi.nt,
#                         p.tb, p.tbr, p.tb.death, p.death, p.emigrate,
#                         transition.matrix = do.call(DefineTransition, arglist.BASELINE.TM))

# Baselines use the same transition matrix
# S0_345 <- S0_12

# New baseline for S1
S0_1 <- DefineStrategy(p.sus,	p.sus.nf,	p.sus.nbt,	p.sus.nct,	p.sus.tc,
                       p.sus.sae,	p.sus.sae.death,
                       p.ltbi,	p.ltbi.nf,	p.ltbi.nbt,	p.ltbi.nct,	p.ltbi.tc,	
                       p.ltbi.sae,	p.ltbi.sae.death,
                       p.tb,	p.tbr,	p.tb.death,	p.death,	p.emigrate,
                       transition.matrix = do.call(DefineTransition, arglist.BASELINE.S1.TM))





# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               RRADJUST = 0.9,
                               # RRADJUST takes into account the fact that a proportion (10% in Victoria)
                               # of TB cases are picked up each year with existing TB control strategies, i.e.
                               # during follow-up as a result of an abnormal CXR during pre-migration off-shore screening.
                               BEGINTREAT = 0.47,
                               # BEGINTREAT = Get.BEGINTREAT(DT, year),
                               # Yet to finalise this parameter. i need to Work out if the chance of beginning treatment 
                               # is age-dependent. May well depend on treatment as well??
                               # TBFOLLOWUPADJUST = Get.TBFOLLOWUPADJUST(DT, year, treatment),
                               ATTEND = 0.836,
                               # Proportion of migrants referred following off-shore screening (CXR) 
                               # that attend follow-up appointment once onshore. 
                               # Source: Flynn MG, Brown LK. Treatment of latent tuberculosis in migrants 
                               # to Victoria. Commun Dis Intell Q Rep 2015; 39(4): E578-83.
                               # SAE = Get.SAE(DT, treatment),
                               # SAEMR = Get.SAEMR(DT, treatment),
                               SAE = 0.0000003,
                               SAEMR = 0.00000004,
                               # EMIGRATE = Get.EMIGRATE(DT, year),
                               EMIGRATE = 0.000000007,
                               TESTSN = Get.TEST(S = "SN", testing),
                               TESTSP = Get.TEST(S = "SP", testing),
                               TESTC = Get.TEST(S = "cost.primary", testing),
                               TREATR = Get.TREAT(S = "rate", treatment),
                               TREATC = Get.TREAT(S = "cost.primary", treatment),
                               # TREATSAE = Get.TREAT(S ="sae", treatment),
                               POP = Get.POP(DT, strategy, markov.cycle),
                               UTILITY = Get.UTILITY(treatment),
                               ATTENDCOST = 200,
                               PARTIALTREATCOST = 400, 
                               TBCOST = 11408.84,
                               SAECOST = 2000
                               )

# Uses aust.vic.rds file to create a sample input
pop.master <- CreatePopulationMaster()

#set.seed(10)
#pop.master <- pop.master[sample(.N, 200)]




#---------- Model parameters for STRATEGY 0 ----------------#

# TODO - Need a new CreatePopulationMaster function to manage LGA column.
discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 5 # Model run cycles

#--------------------- S0_1 ---------------------------#
#---------------Baseline for S1 --------------------#
DoRunModel(S0_1, start.year, cycles)


#--------------------- S0_12 ---------------------------#
#---------------Baseline for S2 --------------------#
# DoRunModel(S0_12, start.year, cycles)

#--------------------- END OF S0_12 ---------------------------#


#-------- Model parameters for S1 & S2 --------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 5 # Model run cycles

DoRunModel(S1, start.year, cycles)

# 
# 
# DoRunModel(S2, start.year, cycles)
# 
# 
# #---------- Model parameters for S3 , S4 & S5----------------#
# 
# discount <- 0.03
# start.year <- 2020
# markov.cycle <- 0 # Tracks the current cycle
# cycles <- 30 # Model run cycles
# 
# DoRunModel(S0_345, start.year, cycles)
# 
# DoRunModel(S3, start.year, cycles)
# DoRunModel(S4, start.year, cycles)
# DoRunModel(S5, start.year, cycles)
# 
# 
# 
# #------------ Manipulating output files------------- #
# 
# 
# # output RDS files on external hard disk
# setwd("D:/")
# 
# 
# 
# CreateOutput("S0_1")
# CreateOutput("S0_12")
# CreateOutput("S0_345")
# CreateOutput("S1")
# CreateOutput("S2")
# CreateOutput("S3")
# CreateOutput("S4")
# CreateOutput("S5")
# 
# # Create output files for PowerBI i.e recombine each type of *.csv file into five  lookup files.
# 
# all.files <- list.files(path = "Data/Output", pattern = "S.csv")
# mylist <- lapply(all.files, Readdata)
# StateCount <- rbindlist(mylist, fill = TRUE)
# rm(mylist)
# fwrite(StateCount, "Data/Output/StateCount.csv")
# rm(StateCount)
# 
# all.files <- list.files(path = "Data/Output", pattern = "SC.csv")
# mylist <- lapply(all.files, Readdata)
# StateCost <- rbindlist(mylist, fill = TRUE)
# rm(mylist)
# fwrite(StateCost, "Data/Output/StateCost.csv")
# rm(StateCost)
# 
# all.files <- list.files(path = "Data/Output", pattern = "F.csv")
# mylist <- lapply(all.files, Readdata)
# FlowCount <- rbindlist(mylist, fill = TRUE)
# rm(mylist)
# fwrite(FlowCount, "Data/Output/FlowCount.csv")
# rm(FlowCount)
# 
# all.files <- list.files(path = "Data/Output", pattern = "FC.csv")
# mylist <- lapply(all.files, Readdata)
# FlowCost <- rbindlist(mylist, fill = TRUE)
# rm(mylist)
# fwrite(FlowCost, "Data/Output/FlowCost.csv")
# rm(FlowCost)
# 
# all.files <- list.files(path = "Data/Output", pattern = "SQ.csv")
# mylist <- lapply(all.files, Readdata)
# StateQALY <- rbindlist(mylist, fill = TRUE)
# rm(mylist)
# fwrite(StateQALY, "Data/Output/StateQALY.csv")
# rm(StateQALY)
