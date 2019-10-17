
# This code needs to replace the "parameter values" script

# parameters I need to get out of this:
  # the incremental qalys and incremental cost

# reactivation rates and any other lookup table:
  # create 10,000 random variables using distribution and length the table by a factor of
  # 10,000 with the variable SIM
  # then when the PSA code needs to retrieve the RRate value, we just have to use the SIM
  # ref too.

options(scipen = 999)
library(data.table)
library(purrr)

WTP = 1000 # willingness to pay threshold
WTP_compare1 = 500
Num_SIm = 10000

# Sourcing useful functions for calculation of distribution parameters
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
source("Distribution parameter calculations.R")

################## PSA #####################################

simrun = c(seq(1, Num_SIm))
simdata = as.data.frame(simrun)

# Define all the parameters that are fixed:
disc <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
startyear <- 2020 # start.year
totalcycles <- 60  # cycles ... The mortality data continues until 2100 and migrant 
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


# Define all the parameters that are uncertain:

# aust, cscreenqft,
# cscreentst10, cscreentst15, ctb, ctreat3HP, ctreat4R, 
# ctreat6H, ctreat9H, emigrate.rate,
# Get.EMIGRATE, Get.MR, Get.POP, Get.RR, Get.SAE,
# Get.SAEMR, Get.TBMR, parttreat3HP, parttreat4R, 
# parttreat6H, parttreat9H, rradj, snqftgit, sntst10, 
# sntst15, spqftgit, sptst10, sptst15, targetfunc, 
# treatr3HP, treatr4R, treatr6H, 
# treatr9H, ttt3HP, ttt4R, ttt6H, ttt9H, uactivetb, 
# uactivetbr, uhealthy, ultbi3HP, ultbi4R, ultbi6H, 
# ultbi9H, ultbipart3HP, ultbipart4R, ultbipart6H, 
# ultbipart9H, ultbitreatsae

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
## probability of rradjust (beta)
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


# For each row of simdata, take the parameters and run the function using the parameters
simdata <- as.data.table(simdata)
parameterlist <- colnames(simdata)
psafunc <- function(begintrt, att, rradj, cattend, csae, cscreenqft,
                    cscreentst, ctb, ctreat3HP, ctreat4R,
                    ctreat6H, ctreat9H, cparttreat3HP, cparttreat4R,
                    cparttreat6H, cparttreat9H, snqftgit, sntst10,
                    sntst15, spqftgit, sptst10, sptst15,
                    treatr3HP, treatr4R, treatr6H,
                    treatr9H, ttt3HP, ttt4R, ttt6H, ttt9H, uactivetb,
                    uactivetbr, uhealthy, ultbi3HP, ultbi4R, ultbi6H,
                    ultbi9H, ultbipart3HP, ultbipart4R, ultbipart6H,
                    ultbipart9H, ultbitreatsae){
  # this function takes all of the parameter values, one from each column,
  
  # The dependent variables need to be defined separately
  
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
    
    emigrate.rate[DT[, .(AGERP)], Rate, on = .(Age = AGERP)]
    
  }
  # Get.MR
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
    
    mid <- vic.tb.mortality[DT[, .(AGEP, SEXP)], Prob, on = .(age = AGEP, sex = SEXP)]
    low <- vic.tb.mortality[DT[, .(AGEP, SEXP)], lowerProb, on = .(age = AGEP, sex = SEXP)]
    high <- vic.tb.mortality[DT[, .(AGEP, SEXP)], upperProb, on = .(age = AGEP, sex = SEXP)]
    betaparam <- findbeta2(mid, low, high)
    rbeta(1, betaparam[1], betaparam[2])
    
  }
  # Get.SAE
  # Look up SAE rate from sae.rate (age and treatment dependent)
  Get.SAE <- function(xDT, treat) {
    
    DT <- copy(xDT[, .(AGERP)])
    
    DT[AGERP > 110, AGERP := 110]
    
    DT$treatment <- as.character(treat)
    
    mid <- sae.rate[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
    low <- sae.rate[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
    high <- sae.rate[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
    betaparam <- findbeta2(mid, low, high)
    rbeta(1, betaparam[1], betaparam[2])
    
  }
  # Get.SAEMR
  # Look up the SAE mortality rate from sae.mortality (age and treatment dependent)
  Get.SAEMR <- function(xDT, treat) {
    
    DT <- copy(xDT[, .(AGERP)])
    
    DT[AGERP > 110, AGERP := 110]
    
    DT$treatment <- as.character(treat)

    mid <- sae.mortality[DT[, .(AGERP, treatment)], Rate, on = .(Age = AGERP, treatment = treatment)]
    low <- sae.mortality[DT[, .(AGERP, treatment)], low, on = .(Age = AGERP, treatment = treatment)]
    high <- sae.mortality[DT[, .(AGERP, treatment)], high, on = .(Age = AGERP, treatment = treatment)]
    betaparam <- findbeta2(mid, low, high)
    rbeta(1, betaparam[1], betaparam[2])
    
  }
  # Below defines parameters for now, but will not be needed eventually
  # setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  # source("CB-TLTBI Parameter values.R")
  # run the CB-TLTBI.R code, i.e. 
  setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
  source("CB-TLTBI.R")
  # then, instead of putting the output into the output folder it needs to use 
  # it to calculate the incremental QALY and incremental cost
  # this step could involve another file? 
  # Finding the baseline quantities
  
  # # Calculating the incremental cost and effectiveness
  # base <- files[[1]]
  # dt <- files[[2]]
  # 
  # # total baseline cost
  # a <- which( colnames(base) == "SC.p.sus" )
  # b <- which( colnames(base) == "SC.p.emigrate" )
  # base$SCsum <- rowSums(base[, a:b], na.rm = TRUE)
  # a <- which( colnames(base) == "FC.p.sus" )
  # b <- which( colnames(base) == "FC.p.emigrate" )
  # base$FCsum <- rowSums(base[, a:b], na.rm = TRUE)
  # totbasecost <- sum(base$SCsum) + sum(base$FCsum)
  # 
  # # total baseline QALYS
  # a <- which( colnames(base) == "SQ.p.sus" )
  # b <- which( colnames(base) == "SQ.p.emigrate" )
  # base$SQsum <- rowSums(base[, a:b], na.rm = TRUE)
  # qalybase <- sum(base$SQsum)
  # 
  # # total cost of strategy
  # a <- which( colnames(dt) == "SC.p.sus" )
  # b <- which( colnames(dt) == "SC.p.emigrate" )
  # dt$SCsum <- rowSums(dt[, a:b], na.rm = TRUE)
  # a <- which( colnames(dt) == "FC.p.sus" )
  # b <- which( colnames(dt) == "FC.p.emigrate" )
  # dt$FCsum <- rowSums(dt[, a:b], na.rm = TRUE)
  # totcost <- sum(dt$SCsum) + sum(dt$FCsum)
  # 
  # # incremental cost of strategy
  # totaddcost <- totcost - totbasecost
  # 
  # # total number of QALYS
  # a <- which( colnames(dt) == "SQ.p.sus" )
  # b <- which( colnames(dt) == "SQ.p.emigrate" )
  # dt$SQsum <- rowSums(dt[, a:b], na.rm =TRUE)
  # qalytot <- sum(dt$SQsum)
  # 
  # #Incremental QALYs
  # incremqaly <- qalytot - qalybase
  # 
  # # Cost per QALY - ICER
  # costperqaly <- totaddcost/incremqaly
  
}

test <- simdata[1:2, ]
test <- as.data.table(test)
# pmap is from the purrr package

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
proc.time() - ptm # Stop the clock
# user  system elapsed 
# 43.29    3.24   54.63 


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


simdata[, lapply(.SD, psafunc), by = group]


## ER visits
simdata$ER_visits_Treatment_A = sample(c(0,1,1,1,2,2,2,3), size = Num_SIm, replace = T)
simdata$ER_visits_Treatment_B = sample(c(0,1,1,1,2,2,2,3), size = Num_SIm, replace = T)
simdata$ER_visits_cost = rnorm(n = Num_SIm, mean = 600, sd = 50)
simdata$ER_visits_cost_Treatment_A = simdata$ER_visits_Treatment_A * simdata$ER_visits_cost ###Treatment_A1
simdata$ER_visits_cost_Treatment_B = simdata$ER_visits_Treatment_B * simdata$ER_visits_cost  

## Hospitalization
simdata$ALOS_Treatment_A = sample(c(0,1,2), size = Num_SIm, replace = T)
simdata$ALOS_Treatment_B = sample(c(0,1,2), size = Num_SIm, replace = T)
simdata$LOS_cost = rnorm(n=Num_SIm, mean = 1600, sd = 150)
simdata$ALOS_cost_Treatment_A = simdata$ALOS_Treatment_A * simdata$LOS_cost ###Treatment_A2 
simdata$ALOS_cost_Treatment_B = simdata$ALOS_Treatment_B * simdata$LOS_cost

## Medication
simdata$units_Treatment_A = sample(c(5,6,7,8,9), size = Num_SIm, replace = T)
simdata$units_Treatment_B = sample(c(5,6,7,8,9), size = Num_SIm, replace = T)
simdata$Treatment_A_unit_cost = rnorm(n=Num_SIm, mean = 481, sd = 10) 
simdata$Treatment_B_unit_cost = rnorm(n=Num_SIm, mean = 170, sd = 10)

simdata$unitS_cost_Treatment_A = simdata$units_Treatment_A * simdata$Treatment_A_unit_cost  ### Treatment_A3
simdata$unitS_cost_Treatment_B = simdata$units_Treatment_B * simdata$Treatment_B_unit_cost

## total Cost
simdata$total_cost_Treatment_A = simdata$unitS_cost_Treatment_A + simdata$ALOS_cost_Treatment_A + simdata$ER_visits_cost_Treatment_A
simdata$total_cost_Treatment_B = simdata$unitS_cost_Treatment_B + simdata$ALOS_cost_Treatment_B + simdata$ER_visits_cost_Treatment_B
simdata$cost_diff = simdata$total_cost_Treatment_A - simdata$total_cost_Treatment_B

## calculate ICER
simdata$icer = simdata$cost_diff / simdata$Effect_prop_diff

write.csv(simdata, "simdata.csv")

# This bit works out whether the colour of the simulations should be green or red
# depending on whether the ICER value is under or over the WTP.

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


#abline(c(0,WTP_compare1), lwd=3)
table(simdata$CE)


# 
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
# 
# 
# 
# lines(p, dbeta(p, 90, 10), type ="l", col = 3)
# lines(p, dbeta(p, 30, 70), col = 2) 
# lines(p, dbeta(p, 3, 7), col = 1) 
# legend(0.2, 30, c("90.92,82545.55", "Be(90,10)", "Be(30, 70)", "Be(3, 7)"),
#        lty = c(1,1,1,1),col=c(4,3,2,1))
# 
# dist <- dbeta(p, 90.92,82545.55)
# dist
# 
# 
# 
# # Create the enormous datatables that will be needed to capture the 
# # uncertainty of the reactivation rates, emigration rates, TB mortality rates,
# # mortality rates, SAE mortality rates and risk of SAE.
# 
# # The strategies and all parameters are defined in the "Parameter values" document.
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# RRates <- readRDS("Data/RRatescobincidnosex.rds")
# # taking into account the uncertainty
# finddistfunc <- function(x){
#   betaparam <- findbeta2(RRates$Rate, RRates$LUI, RRates$UUI)
#   rbeta(Num_SIm, betaparam[1], betaparam[2])
# }
# 
# testbase <- RRates[1:4, ]
# 
# testresult <- apply(testbase, 1, finddistfunc)
# 
# 
# RRates[, finddistfunc(simrun, begin, attend), by = seq_len(nrow(RRates))]
# 
# begintrt, 
# att, 
# rradj, 
# cattend, 
# csae, 
# cscreenqft,
# cscreentst, 
# ctb, 
# ctreat3HP, 
# ctreat4R,
# ctreat6H, 
# ctreat9H, 
# cparttreat3HP, 
# cparttreat4R,
# cparttreat6H, 
# cparttreat9H, 
# Get.EMIGRATE, 
# Get.MR, 
# Get.POP, 
# Get.RR, 
# Get.SAE,
# Get.SAEMR, 
# Get.TBMR, 
# snqftgit, 
# sntst10,
# sntst15, 
# spqftgit, 
# sptst10, 
# sptst15, 
# targetfunc,
# treatr3HP, 
# treatr4R, 
# treatr6H,
# treatr9H, 
# ttt3HP, 
# ttt4R, 
# ttt6H, 
# ttt9H, 
# uactivetb,
# uactivetbr, 
# uhealthy, 
# ultbi3HP, 
# ultbi4R, 
# ultbi6H,
# ultbi9H, 
# ultbipart3HP, 
# ultbipart4R, 
# ultbipart6H,
# ultbipart9H, 
#ultbitreatsae <- ultbitreatsae

