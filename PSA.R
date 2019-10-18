
# This is the code for the probabilistic sensitivity analysis (PSA)
# The "Parameter values" script is not needed for PSA
# All parameter values are defined below

# This script should run the modl 10,000 times
# and output the incremental qalys and incremental costs of each run
# so that the cost-effectiveness plane and acceptability curves can be plotted.

options(scipen = 999)
library(data.table)
library(purrr)
library(ggplot2)
library(scales)

################## PSA #####################################

# Defining the number of simulations
Num_SIm = 10000

# Sourcing useful functions for calculation of parameter distributions
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("Distribution parameter calculations.R")

# Create a datatable that will eventually contain the
# parameter values to be used for each simulation/model run.
simrun = c(seq(1, Num_SIm))
simdata = as.data.frame(simrun)

# Define all the parameters that are don't have any uncertainty:
disc <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
startyear <- 2020 # start.year
totalcycles <- 30  # cycles ... The mortality data continues until 2100 and migrant 
# inflows are possible until 2050
finalyear <- startyear + totalcycles

# The tests and treatments I want to consider in the run
testlist <- c("QTFGIT") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("3HP") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# MIGRANT INFLOWS
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


# Define all the parameters that are uncertain, but that
# are also fixed for each model run (i.e. they aren't dependent
# on age or year etc).

# begintrt
# probability of beginning treatment (beta)
mean <- 0.596
low <- 0.331
high <- 0.8
betaparam <- findbeta2(mean, low, high)
simdata$begintrt = rbeta(Num_SIm, betaparam[1], betaparam[2])

# att
# probability of attending follow-up (beta)
mean <- 0.836
low <- 0.5
high <- 1.0
betaparam <- findbeta2(mean, low, high)
simdata$att = rbeta(Num_SIm, betaparam[1], betaparam[2])

# rradj
# probability of rradjust (beta)
mean <- 0.9
low <- 0.952
high <- 0.875
betaparam <- findbeta2(mean, low, high)
simdata$rradj = rbeta(Num_SIm, betaparam[1], betaparam[2])

# COSTS

# cattend
# cost of attending treatment (gamma??)
mean <- 103.48
low <- 103.48
high <- 103.48
betaparam <- findgamma2(mean, low, high)
simdata$cattend = rgamma(Num_SIm, betaparam[1], betaparam[2])

# csae
# cost of severe adverse event (gamma??)
mean <- 1124
low <- 1124
high <- 1124
betaparam <- findgamma2(mean, low, high)
simdata$csae = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cscreenqft
# cost of QFT screen (gamma??)
mean <- 1124
low <- 1124
high <- 1124
betaparam <- findgamma2(mean, low, high)
simdata$cscreenqft = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cscreentst
# cost of TST screen (gamma??)
mean <- 1124
low <- 1124
high <- 1124
betaparam <- findgamma2(mean, low, high)
simdata$cscreentst = rgamma(Num_SIm, betaparam[1], betaparam[2])

# ctb
# cost of active TB (gamma) WRONG!!!
mean <- 12000
low <- 5000
high <- 178000
betaparam <- findgamma2(mean, low, high)
simdata$ctb = rgamma(Num_SIm, betaparam[1], betaparam[2])

# ctreat3HP 
# cost of 3HP (gamma) WRONG!!!
mean <- 208.03
low <- 50
high <- 300
betaparam <- findgamma2(mean, low, high)
simdata$ctreat3HP = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cparttreat3HP
# cost of partial 3HP (gamma) WRONG!!!
mean <- 82.81
low <- 50
high <- 100
betaparam <- findgamma2(mean, low, high)
simdata$cparttreat3HP = rgamma(Num_SIm, betaparam[1], betaparam[2])

# ctreat4R
# cost of 4R (gamma) WRONG!!!
mean <- 431.41
low <- 300
high <- 500
betaparam <- findgamma2(mean, low, high)
simdata$ctreat4R = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cparttreat4R
# cost of partial 4R (gamma) WRONG!!!
mean <- 157.82
low <- 50
high <- 200
betaparam <- findgamma2(mean, low, high)
simdata$cparttreat4R = rgamma(Num_SIm, betaparam[1], betaparam[2])

# ctreat6H
# cost of 6H (gamma) WRONG!!!
mean <- 239.53
low <- 50
high <- 300
betaparam <- findgamma2(mean, low, high)
simdata$ctreat6H = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cparttreat6H
# cost of partial 6H (gamma) WRONG!!!
mean <- 97.65
low <- 50
high <- 200
betaparam <- findgamma2(mean, low, high)
simdata$cparttreat6H = rgamma(Num_SIm, betaparam[1], betaparam[2])

# ctreat9H
# cost of 9H (gamma) WRONG!!!
mean <- 342.79
low <- 50
high <- 400
betaparam <- findgamma2(mean, low, high)
simdata$ctreat9H = rgamma(Num_SIm, betaparam[1], betaparam[2])

# cparttreat9H
# cost of partial 9H (gamma) WRONG!!!
mean <- 138.23
low <- 50
high <- 200
betaparam <- findgamma2(mean, low, high)
simdata$cparttreat9H = rgamma(Num_SIm, betaparam[1], betaparam[2])


# SENSITIVITIES AND SPECIFICITIES

# snqftgit
## sensitivity of the qftgit (beta)
mean <- 0.6104
low <- 0.4925
high <- 0.7195
betaparam <- findbeta2(mean, low, high)
simdata$snqftgit = rbeta(Num_SIm, betaparam[1], betaparam[2])

# spqftgit
## probability of rradjust (beta)
mean <- 0.95820
low <- 0.7784
high <- 1
betaparam <- findbeta2(mean, low, high)
simdata$spqftgit = rbeta(Num_SIm, betaparam[1], betaparam[2])

# sntst15
## probability of rradjust (beta)
mean <- 0.6341
low <- 0.5590
high <- 0.7777
betaparam <- findbeta2(mean, low, high)
simdata$sntst15 = rbeta(Num_SIm, betaparam[1], betaparam[2])

# sptst15
## probability of rradjust (beta)
mean <- 0.95117
low <- 0.7726
high <- 1
betaparam <- findbeta2(mean, low, high)
simdata$sptst15 = rbeta(Num_SIm, betaparam[1], betaparam[2])

# sntst10
## probability of rradjust (beta)
mean <- 0.7532
low <- 0.6418
high <- 0.8444
betaparam <- findbeta2(mean, low, high)
simdata$sntst10 = rbeta(Num_SIm, betaparam[1], betaparam[2])

# sptst10
## probability of rradjust (beta)
mean <- 0.82227
low <- 0.6562
high <- 1
betaparam <- findbeta2(mean, low, high)
simdata$sptst10 = rbeta(Num_SIm, betaparam[1], betaparam[2])

# TREATR

# treatr3HP 
# cost of 3HP (gamma) WRONG!!!
mean <- 0.74088
low <- 0.5
high <- 1
betaparam <- findgamma2(mean, low, high)
simdata$treatr3HP = rgamma(Num_SIm, betaparam[1], betaparam[2])

# treatr4R
# cost of 4R (gamma) WRONG!!!
mean <- 0.82272
low <- 0.5
high <- 1
betaparam <- findgamma2(mean, low, high)
simdata$treatr4R = rgamma(Num_SIm, betaparam[1], betaparam[2])

# treatr6H
# cost of 6H (gamma) WRONG!!!
mean <- 0.5568
low <- 0.5
high <- 1
betaparam <- findgamma2(mean, low, high)
simdata$treatr6H = rgamma(Num_SIm, betaparam[1], betaparam[2])

# treatr9H
# cost of 9H (gamma) WRONG!!!
mean <- 0.735800
low <- 0.5
high <- 1
betaparam <- findgamma2(mean, low, high)
simdata$treatr9H = rgamma(Num_SIm, betaparam[1], betaparam[2])

# TIME TO TREATMENT

# ttt3HP
## Time to complete LTBI treatment (beta)
mean <- 0.375
low <- 0.292
high <- 0.500
betaparam <- findbeta2(mean, low, high)
simdata$ttt3HP = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ttt4R
## Time to complete LTBI treatment (beta)
mean <- 0.458
low <- 0.375
high <- 0.583
betaparam <- findbeta2(mean, low, high)
simdata$ttt4R = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ttt6H
## Time to complete LTBI treatment (beta)
mean <- 0.625
low <- 0.542
high <- 0.750
betaparam <- findbeta2(mean, low, high)
simdata$ttt6H = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ttt9H
## Time to complete LTBI treatment (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ttt9H = rbeta(Num_SIm, betaparam[1], betaparam[2])

#UTILITIES

# uactivetb
## utility of active TB (beta)
mean <- 0.780730111
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$uactivetb = rbeta(Num_SIm, betaparam[1], betaparam[2])

# uactivetbr
## utility of recovered TB (beta)
mean <- 0.876
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$uactivetbr = rbeta(Num_SIm, betaparam[1], betaparam[2])

# uhealthy
## utility of healthy state (beta)
mean <- 0.876
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$uhealthy = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbitreatsae
## utility of SAE (beta)
mean <- 0.8343333
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbitreatsae = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbi3HP
## utility of 3HP (beta)
mean <- 0.876
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbi3HP = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbi4R
## utility of 4R (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbi4R = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbi6H
## utility of 6H (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbi6H = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbi9H
## utility of 9H (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbi9H = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbipart3HP
## utility of part 3HP (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbipart3HP = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbipart4R
## utility of part 4R (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbipart4R = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbipart6H
## utility of part 6H (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbipart6H = rbeta(Num_SIm, betaparam[1], betaparam[2])

# ultbipart9H
## utility of part 9H (beta)
mean <- 0.875
low <- 0.792
high <- 1.00
betaparam <- findbeta2(mean, low, high)
simdata$ultbipart9H = rbeta(Num_SIm, betaparam[1], betaparam[2])


simdata <- as.data.table(simdata)

snqftgit <- simdata[2, snqftgit]

# The function below attempts to...
# for each row of the simdata datatable, 
# take the parameter values from that row 
# and run "CB-TLTBI.R" 
psafunc <- function(simnumber){
  parameters <- c("begintrt", "att", "rradj", "cattend", "csae", "cscreenqft", 
                     "cscreentst", "ctb", "ctreat3HP", "ctreat4R",
                     "ctreat6H", "ctreat9H", "cparttreat3HP", "cparttreat4R",
                     "cparttreat6H", "cparttreat9H", "snqftgit", "sntst10",
                     "sntst15", "spqftgit", "sptst10", "sptst15",
                     "treatr3HP", "treatr4R", "treatr6H",
                     "treatr9H", "ttt3HP", "ttt4R", "ttt6H", "ttt9H", "uactivetb",
                     "uactivetbr", "uhealthy", "ultbi3HP", "ultbi4R", "ultbi6H",
                     "ultbi9H", "ultbipart3HP", "ultbipart4R", "ultbipart6H",
                     "ultbipart9H", "ultbitreatsae")
  # getparams <- function(x){
  #   x <- simdata[simnumber, ..x]
  # }
  # lapply(parameters, getparams)
  
  snqftgit <- simdata[simnumber, snqftgit]
  sntst10 <- simdata[simnumber, sntst10]
  sntst15 <- simdata[simnumber, sntst15]
  
  spqftgit <- simdata[simnumber, spqftgit]
  sptst10 <- simdata[simnumber, sptst10]
  sptst15 <- simdata[simnumber, sptst15]
  
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
    rbeta(1, betaparam[1], betaparam[2])
    
  } 
  # Get.EMIGRATE
  emigrate.rate <- readRDS("Data/emigrate.rate.rds") # BASELINE assumed rate incorporating both temp and permanent residents 
  # emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds") # LOWER assumed rate among permanent residents
  emigrate.rate <- as.data.table(emigrate.rate)
  
  Get.EMIGRATE <- function(xDT, year) {
    
    DT <- copy(xDT[, .(year, AGERP, YARP)])
    
    DT[AGERP > 110, AGERP := 110]
    
    mid <- emigrate.rate[DT[, .(AGERP)], Rate, on = .(Age = AGERP)]
    low <- emigrate.rate[DT[, .(AGERP)], lower, on = .(Age = AGERP)]
    high <- emigrate.rate[DT[, .(AGERP)], upper, on = .(Age = AGERP)]
    betaparam <- findbeta2(mid, low, high)
    rbeta(1, betaparam[1], betaparam[2])
    
  }
  # Get.MR ##### Do I need to incorporate uncertainty for this parameter?
  # Look up the mortality rate from vic.mortality
  Get.MR <- function(xDT, year, rate.assumption = "Med") {
    
    DT <- copy(xDT[, .(AGEP, SEXP)])
    
    # To lookup all ages beyond 110
    DT[AGEP > 100, AGEP := 100]
    
    vic.mortality[Year == year & mrate == rate.assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]
    
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
    rbeta(1, betaparam[1], betaparam[2])
    
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
    rbeta(1, betaparam[1], betaparam[2])
    
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
    rbeta(1, betaparam[1], betaparam[2])
    
  }
  
  # run the CB-TLTBI.R code, i.e. 
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  source("CB-TLTBI.R")
  
}

simdata <- simdata[1:2, ]
simdata <- as.data.table(simdata)

Num_SIm <- 2

# To execute the psafunc function and model runs I could try...
out <- lapply(Num_SIm, psafunc)




ptm <- proc.time() # Start the clock!
test[, value := pmap(.(begintrt, att, rradj, cattend, csae, cscreenqft, 
                       cscreentst, ctb, ctreat3HP, ctreat4R,
                       ctreat6H, ctreat9H, cparttreat3HP, cparttreat4R,
                       cparttreat6H, cparttreat9H, snqftgit, sntst10,
                       sntst15, spqftgit, sptst10, sptst15,
                       treatr3HP, treatr4R, treatr6H,
                       treatr9H, ttt3HP, ttt4R, ttt6H, ttt9H, uactivetb,
                       uactivetbr, uhealthy, ultbi3HP, ultbi4R, ultbi6H,
                       ultbi9H, ultbipart3HP, ultbipart4R, ultbipart6H,
                       ultbipart9H, ultbitreatsae), psafunc)]
# pmap is from the purrr package
proc.time() - ptm # Stop the clock
# user  system elapsed 
# 43.29    3.24   54.63 


# To execute the models runs I could try...




ptm <- proc.time() # Start the clock!
test[, psafunc(begintrt, att, rradj, cattend, csae, cscreenqft,
               cscreentst, ctb, ctreat3HP, ctreat4R,
               ctreat6H, ctreat9H, cparttreat3HP, cparttreat4R,
               cparttreat6H, cparttreat9H, snqftgit, sntst10,
               sntst15, spqftgit, sptst10, sptst15,
               treatr3HP, treatr4R, treatr6H,
               treatr9H, ttt3HP, ttt4R, ttt6H, ttt9H, uactivetb,
               uactivetbr, uhealthy, ultbi3HP, ultbi4R, ultbi6H,
               ultbi9H, ultbipart3HP, ultbipart4R, ultbipart6H,
               ultbipart9H, ultbitreatsae), by = seq_len(nrow(test))]
proc.time() - ptm # Stop the clock
# user  system elapsed 
# 44.05    2.37   59.23 





# Plot of PSA results on cost effectiveness plane.
# The code below will plot the 10,000 model run outputs on a
# cost effectiveness plane.
# A blue willingness to pay line is drawn on the plane too
# and the colour of the simulations will be either green or red
# depending on whether the ICER value is under or over the WTP.

WTP = 1000 # willingness to pay threshold
WTP_compare1 = 500

simdata$model = WTP * simdata$Effect_prop_diff 

simdata$model_true = simdata$model - simdata$cost_diff 

simdata$CE = ifelse(test = simdata$model_true > 0,yes = 1, no = 0 )

simdata$CE_col = ifelse(test = simdata$CE == 0, yes = 2, no = 3 )
table(simdata$CE)

plot(simdata$cost_diff ~ simdata$Effect_prop_diff, 
     col = simdata$CE_col, cex = .8, pch = 3,
     xlim = c(-30, 30), ylim = c(-8000, 8000))
abline(h = 0, lwd = 2 )
abline(v = 0, lwd = 2 )
abline(c(0, WTP), col = 4, lwd = 3)

# #abline(c(0,WTP_compare1), lwd=3)
# table(simdata$CE)


# Plot of acceptability curve
# work out the proportion cost-effective
icerlist <- simdata$icer
maxwtp <- 100000
wtp <- c(0:maxwtp)
propcosteffectivefunc <- function(wtp){
  (sum(icerlist < wtp)/10000) * 100
}
propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))

# create a new table, the accepty table 
accepty <- data.frame(wtp, propcosteffect)

# plot
myplot1 <- 
  ggplot(accepty, aes(x = wtp, y = propcosteffect))+
  geom_line(size = 1, colour = "blue") +
  labs(x = "Willingness-to-pay threshold (AUD$)", 
       y = "Proportion cost-effective (%)") +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_x_continuous(label = comma, breaks = seq(0, 150000, 20000)) +
  theme_bw() +
  theme(text = element_text(size = 25))
        # panel.border = element_blank(),
        # legend.position = "right",
        # axis.text.x = element_text(angle = 45, hjust = 1))
        # axis.text.y = element_text(size = 12))
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
tiff('acceptability.tiff', 
     units = "in", width = 15, height = 12,
     res = 600)
myplot1
dev.off()




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
