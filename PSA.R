
# This is the code for the probabilistic sensitivity analysis (PSA)

# The "CB-TLTBI.R" and "Parameter values" scripts are not needed for PSA...
# ...the model setup and parameter values are all located within this file, below.

# This script should run the model "Num_SIm" times
# and output incremental qalys and incremental costs for each run
# so that the cost-effectiveness plane and acceptability curves can be plotted.

# Parameters that aren't dependent on other variables (age, year of arrival, year etc)
# are defined in the RDS file called "params offshore" and "params onshore" 
# in the main folder. These files are created using the "Parameter creation..." scripts

# Some parameters in this file do have to be altered, depending on the values
# of other parameters.


# Coding style
# https://google.github.io/styleguide/Rguide.xml

# This will convert any warning into an error and will stop the loop
options(warn = 2)
# To restore to the original setting use options("warn"=0) or options(warn=1)

options(scipen = 999)
library(data.table)
library(purrr)
library(ggplot2)
library(scales)
library(lazyeval) 
library(data.table) 
library(doParallel)
library(foreach)
library(mc2d)
library(dplyr)
library(epitools)
library(plyr)
library(reshape2)
library(tidyr)


# Sourcing required functions from other scripts
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
source("CB-TLTBI Functions.R") # contains many functions necessary to run the model
source("CB-TLTBI_DataPreparation.R") # for sorting the population data
source("Distribution parameter calculations.R") # for determining distribution parameter values

################## PSA #####################################

# Defining the number of simulations we want
Num_SIm <- 5

# Generating a random set of numbers, one for each simulation
# that will be used as a seed number for "set.seed" functions
# below, so that the sampling from distributions is the same
# for both the baseline and strategy model runs
set.seed(10000)
set.seed.number <- sample(1000:1000000, 10000, replace = F)

# Create a datatable that will eventually contain the
# parameter values to be used for each simulation/model run.
simrun <- c(seq(1, Num_SIm))
simdata <- as.data.frame(simrun)
simdata <- as.data.table(simdata)
 
# Define all the parameters which we aren't varying:
discount <- 0.03 # discount rate baseline 0.03, low 0.00, high 0.05
start.year <- 2020 # start.year
totalcycles <- 98  # cycles ... The mortality data continues until 2100 and migrant 
cycles <- totalcycles 
# inflows are possible until 2050
final.year <- start.year + totalcycles

kill.off.above <- 120 # age above which all enter death state

# The tests and treatments I want to consider in this analysis
testlist <- c("TST10") # baseline c("QTFGIT", "TST10", "TST15"), for sensitivity analysis c("TST15") 
treatmentlist <- c("4R") # baseline c("4R", "3HP", "6H", "9H"), for sensitivity analysis c("3HP")

# The number of migrants I want to include in each inflow
migrant.inflow.size <- 434340 # 440980
# the migrant inflow will stop after the following Markov cycle
finalinflow <- 0

# Get.POP
# This defines the target population
Get.POP <- function(DT, strategy) {
  
  # 200+
  (ifelse(DT[, ISO3] == "200+", 1, 0)) & 
  # 150+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0)) & 
  # 100+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0)) &
  # 40+
  # (ifelse(DT[, ISO3] == "200+", 1, 0) | ifelse(DT[, ISO3] == "150-199", 1, 0) | ifelse(DT[, ISO3] == "100-149", 1, 0) | ifelse(DT[, ISO3] == "40-99", 1, 0)) &
    # Adjust age
    (ifelse(DT[, AGERP] > 10, 1, 0) &
       ifelse(DT[, AGERP] < 66, 1, 0))
  
}

# Define all the parameters that are uncertain, but that
# are also fixed for each model run 
# (i.e. they aren't dependent on age or year etc).
# the upper and lower limits for each
# is defined in the relevant params.rds file and needs to be read in:
# read in parameter list and values, which is defined in the "Parameter creation" scripts
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
dt <- readRDS("params onshore.rds")
onshore <- 1

# dt <- readRDS("params offshore.rds")
# onshore <- 0
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################

# Read in the migrant population data, showing how many have LTBI in each population group.
austbase <- readRDS("Data/Aust16.psa.rds") # baseline


# reformat data table
dt <- as.data.table(dt)
setnames(dt, "p", "abbreviation")
dt[, abbreviation := as.character(abbreviation)]
dt[, mid := as.numeric(as.character(mid))]
dt[, low := as.numeric(as.character(low))]
dt[, high := as.numeric(as.character(high))]
dt[, distribution := as.character(distribution)]

# The loop below adds one column to the simdata table for each parameter value
# defined in the rows of the params....rds file (these are parameters that aren't dependent
# on other variables, i.e. they are fixed for each run). 
# The loop below uses the upper and lower limits in the
# file, together with the distribution defined in the "distribution" column, 
# to calculate a distribution and then take 10,000 samples from it
# that represents the parameter uncertainty

for(i in 1:nrow(dt)) {
  abbreviation <- dt[i, abbreviation]
  mid <- dt[i, mid]
  low <- dt[i, low]
  high <- dt[i, high]
  shape <- dt[i, shape]
  distribution <- dt[i, distribution]
  if (distribution == "pert") {
    simdata[, newcol := rpert(Num_SIm, min = low, mode = mid,
                              max = high, shape = shape)]
    setnames(simdata, "newcol", abbreviation)
  }
  # else if (distribution == "gamma") {
  #   betaparam <- findgamma2(mid, low, high)
  #   simdata[, newcol := rgamma(Num_SIm, betaparam[1], betaparam[2])]
  #   setnames(simdata, "newcol", abbreviation)
  # }
  else if (distribution == "beta") {
    betaparam <- findbeta2(mid, low, high)
    simdata[, newcol := rbeta(Num_SIm, betaparam[1], betaparam[2])]
    setnames(simdata, "newcol", abbreviation)
  }
  else if (distribution == "uniform") {
    simdata[, newcol := runif(Num_SIm, min = low, max = high)]
    setnames(simdata, "newcol", abbreviation)
  }
  else {
    simdata[, newcol := 0]
    setnames(simdata, "newcol", abbreviation)
  }
}



# Plotting the parameter table and distributions used for all of the different

# Write the table to clipboard so I can paste it into Excel
write.table(dt, file = "clipboard-16384", sep = "\t", row.names = FALSE)


# parameters
#plotting transitions
dtcopy <- copy(dt)
dtcopy <- as.data.table(dtcopy)
dtcopy <- subset(dtcopy, abbreviation != "cscreenqft")
dtcopy <- subset(dtcopy, abbreviation != "cscreentst")
a <- which( dtcopy$abbreviation == "attscreen" )
b <- which( dtcopy$abbreviation == "num.appt9H" )
plot.dt <- dtcopy[c(a:b)]
nrow(plot.dt)
# dev.off()
# set up the plotting space
#layout(matrix(1:nrow(plot.dt), ncol = 6))
par(mfrow = c(2, 3))
for(i in 1:nrow(plot.dt)) {
  # store data in column.i as x
  abbreviation <- plot.dt[i, abbreviation]
  mid <- plot.dt[i, mid]
  low <- plot.dt[i, low]
  high <- plot.dt[i, high]
  shape <- plot.dt[i, shape]
  distribution <- plot.dt[i, distribution]
  plotnum <- paste("plot", i, sep = "")
  if (high < 1){
    upperlim <- 1
  }
  else {
    upperlim <- high + 20
  }
  if (distribution == "beta") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dbeta(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "uniform") {
    p = seq(0, upperlim, length = 1000)
    plot(p, dunif(p, min = low, max = high),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "gamma") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dgamma(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else {
    p = seq(0, upperlim, length = 1000)
    plot(p, dpert(p, min = low, mode = mid,
                  max = high, shape = shape),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         #ylim = c(0, 1),
         main = abbreviation)
  }
}


a <- which( dt$abbreviation == "begintrt" )
b <- which( dt$abbreviation == "sptst10" )
plot.dt <- dt[a:b,]
nrow(plot.dt)
# dev.off()
# set up the plotting space
#layout(matrix(1:nrow(plot.dt), ncol = 6))
par(mfrow = c(3, 3))
for(i in 1:nrow(plot.dt)) {
  # store data in column.i as x
  abbreviation <- plot.dt[i, abbreviation]
  mid <- plot.dt[i, mid]
  low <- plot.dt[i, low]
  high <- plot.dt[i, high]
  shape <- plot.dt[i, shape]
  distribution <- plot.dt[i, distribution]
  plotnum <- paste("plot", i, sep = "")
  if (high < 1){
    upperlim <- 1
  }
  else {
    upperlim <- high + 20
  }
  if (distribution == "beta") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dbeta(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "uniform") {
    p = seq(0, upperlim, length = 1000)
    plot(p, dunif(p, min = low, max = high),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "gamma") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dgamma(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else {
    p = seq(0, upperlim, length = 1000)
    plot(p, dpert(p, min = low, mode = mid,
                  max = high, shape = shape),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         #ylim = c(0, 1),
         main = abbreviation)
  }
}

#plotting more transitions - a
a <- which( dt$abbreviation == "treat.complete.3HP" )
b <- which( dt$abbreviation == "ttt9H" )
plot.dt <- dt[a:b,]
nrow(plot.dt)
dev.off()
# set up the plotting space
par(mfrow = c(3, 4))
#layout(matrix(1:nrow(plot.dt), ncol = 11))
for(i in 1:nrow(plot.dt)) {
  # store data in column.i as x
  abbreviation <- plot.dt[i, abbreviation]
  mid <- plot.dt[i, mid]
  low <- plot.dt[i, low]
  high <- plot.dt[i, high]
  shape <- plot.dt[i, shape]
  distribution <- plot.dt[i, distribution]
  plotnum <- paste("plot", i, sep = "")
  if (high < 1){
    upperlim <- 1
  }
  else {
    upperlim <- high + 20
  }
  if (distribution == "beta") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dbeta(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "uniform") {
    p = seq(0, upperlim, length = 1000)
    plot(p, dunif(p, min = low, max = high),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "gamma") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dgamma(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else {
    p = seq(0, upperlim, length = 1000)
    plot(p, dpert(p, min = low, mode = mid,
                  max = high, shape = shape),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
}

#plotting costs
a <- which( dt$abbreviation == "csae" )
b <- which( dt$abbreviation == "cmed9H" )
plot.dt <- dt[a:b,]
nrow(plot.dt)
dev.off()
# set up the plotting space
par(mfrow = c(2, 3))
#layout(matrix(1:nrow(plot.dt), ncol = 11))
for(i in 1:nrow(plot.dt)) {
  # store data in column.i as x
  abbreviation <- plot.dt[i, abbreviation]
  mid <- plot.dt[i, mid]
  low <- plot.dt[i, low]
  high <- plot.dt[i, high]
  shape <- plot.dt[i, shape]
  distribution <- plot.dt[i, distribution]
  plotnum <- paste("plot", i, sep = "")
  if (high < 1){
    upperlim <- 1
  }
  else {
    upperlim <- high + 20
  }
  if (distribution == "beta") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dbeta(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "uniform") {
    p = seq(0, upperlim, length = 1000)
    plot(p, dunif(p, min = low, max = high),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else if (distribution == "gamma") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dgamma(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
  else {
    p = seq(0, upperlim, length = 1000)
    plot(p, dpert(p, min = low, mode = mid,
                  max = high, shape = shape),
         ylab = "density", type = "l", col = 4, xlim = c(0, high),
         main = abbreviation)
  }
}

#plotting utilities
a <- which( dt$abbreviation == "uactivetb" )
b <- which( dt$abbreviation == "ultbi9H" )
plot.dt <- dt[a:b,]
nrow(plot.dt)
dev.off()
# set up the plotting space
par(mfrow = c(2, 4))
for(i in 1:nrow(plot.dt)) {
  # store data in column.i as x
  abbreviation <- plot.dt[i, abbreviation]
  mid <- plot.dt[i, mid]
  low <- plot.dt[i, low]
  high <- plot.dt[i, high]
  shape <- plot.dt[i, shape]
  distribution <- plot.dt[i, distribution]
  plotnum <- paste("plot", i, sep = "")
  if (high < 1){
    upperlim <- 1
  }
  else {
    upperlim <- high + 20
  }
  if (distribution == "beta") {
    p = seq(0., upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dbeta(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0.7, high),
         main = abbreviation)
  }
  else if (distribution == "gamma") {
    p = seq(0, upperlim, length = 1000)
    betaparam <- findbeta2(mid, low, high)
    plot(p, dgamma(p, betaparam[1], betaparam[2]),
         ylab = "density", type = "l", col = 4, xlim = c(0.7, high),
         main = abbreviation)
  }
  else if (distribution == "uniform") {
    p = seq(0, upperlim, length = 1000)
    plot(p, dunif(p, min = low, max = high),
         ylab = "density", type = "l", col = 4, xlim = c(0.7, high),
         main = abbreviation)
  }
  else {
    p = seq(0, upperlim, length = 1000)
    plot(p, dpert(p, min = low, mode = mid,
                  max = high, shape = shape),
         ylab = "density", type = "l", col = 4, xlim = c(0.7, high),
         main = abbreviation)
  }
}
# Restore margins...could also do it with dev.off()  ?
par(mfrow = c(1,1))

# Adjusting the partial LTBI treatment utilities so 
# they are dependent on the value of
# the sampled utility for full treatment
part.utility.dec <- 0.5
simdata[, ultbipart3HP := uhealthy - ((uhealthy - ultbi3HP) * part.utility.dec)]
simdata[, ultbipart4R := uhealthy - ((uhealthy - ultbi4R) * part.utility.dec)]
simdata[, ultbipart6H := uhealthy - ((uhealthy - ultbi6H) * part.utility.dec)]
simdata[, ultbipart9H := uhealthy - ((uhealthy - ultbi9H) * part.utility.dec)]

# Adjusting the costs in the simdata table
# based on the values of other parameters in the
# simdata table
# Sourcing the medical costs
source("Medical costs.R")


# These specify how much of the appointment and medicine
# costs are applied for the partial costs and treatment
part.appt <- 2
part.med <- 3

proportion.nonvr <- 0.137

c.gp.first <- c.gp.c.vr * (1 - proportion.nonvr) + c.gp.c.nonvr * proportion.nonvr

c.gp.review <- c.gp.b.vr * (1 - proportion.nonvr) + c.gp.b.nonvr * proportion.nonvr

chance.of.needing.mcs <- 0.1


# Cost of initial appointment after positive screen
# is fixed for the offshore scenario
# and varies for the onshore scenario depending on the 
# value of the prop.spec. So this value must be defined 
# before the cattend can be defined

if (onshore == 0) {
  
} else if (onshore == 1) {
  
  simdata[, cattend := ((c.gp.review + (c.mcs * chance.of.needing.mcs) +
                           c.cxr) * (1 - prop.spec)) + 
            ((c.spec.first + (c.mcs * chance.of.needing.mcs) +
                c.cxr) * prop.spec)]
}

# Cost of specialist appointments during 
# treatment will all be review appointments 
# for the onshore screening scenarios only,
# becuse the first review would have 
# occurred in the initial appointment after positive screen

if (onshore == 1) {
  c.spec.first <- c.spec.review
} 


# 3HP sort
simdata[, appt := num.appt3HP * c.gp.review + c.liver]
simdata[, spec.appt := c.spec.first + (num.appt3HP - 1) * c.spec.review + c.liver]
simdata[, ctreat3HP := appt + cmed3HP]
simdata[, cparttreat3HP :=  appt / part.appt + cmed3HP / part.med]      
simdata[, ctreatspec3HP :=  spec.appt + cmed3HP] 
simdata[, cparttreatspec3HP :=  spec.appt / part.appt + cmed3HP / part.med]

# 4R sort
simdata[, appt := num.appt4R * c.gp.review]
simdata[, spec.appt := c.spec.first + (num.appt4R - 1) * c.spec.review]
simdata[, ctreat4R := appt + cmed4R]
simdata[, cparttreat4R :=  appt / part.appt + cmed4R / part.med]      
simdata[, ctreatspec4R :=  spec.appt + cmed4R] 
simdata[, cparttreatspec4R :=  spec.appt / part.appt + cmed4R / part.med]

# 6H sort
simdata[, appt := num.appt6H * c.gp.review + c.liver]
simdata[, spec.appt := c.spec.first + (num.appt6H - 1) * c.spec.review + c.liver]
simdata[, ctreat6H := appt + cmed6H]
simdata[, cparttreat6H :=  appt / part.appt + cmed6H / part.med]      
simdata[, ctreatspec6H :=  spec.appt + cmed6H] 
simdata[, cparttreatspec6H :=  spec.appt / part.appt + cmed6H / part.med]

# 9H sort
simdata[, appt := num.appt9H * c.gp.review + c.liver]
simdata[, spec.appt := c.spec.first + (num.appt9H - 1) * c.spec.review + c.liver]
simdata[, ctreat9H := appt + cmed9H]
simdata[, cparttreat9H :=  appt / part.appt + cmed9H / part.med]      
simdata[, ctreatspec9H :=  spec.appt + cmed9H] 
simdata[, cparttreatspec9H :=  spec.appt / part.appt + cmed9H / part.med] 


# Sort the TREATR value for each row of the simdata table
# because this doesn't need to be calcuated within the run

treatrcalc3HP <- function(treat.complete, treat.effic) {
  
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
  
}

treatrcalc4R <- function(treat.complete, treat.effic) {
  
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
  
}

treatrcalc6H <- function(treat.complete, treat.effic) {
  
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
  
}

treatrcalc9H <- function(treat.complete, treat.effic) {
  
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
  
}

simdata[, treatr3HP :=  treatrcalc3HP(treat.complete.3HP, treat.effic.3HP)] 
simdata[, treatr4R :=  treatrcalc4R(treat.complete.3HP, treat.effic.3HP)] 
simdata[, treatr6H :=  treatrcalc6H(treat.complete.6H, treat.effic.6H)] 
simdata[, treatr9H :=  treatrcalc9H(treat.complete.9H, treat.effic.9H)] 

# Get.RR
# Reactivation rates
Get.RR <- function(xDT, year) {
  
  DT <- copy(xDT[, .(AGERP, YARP, ISO3, AGEP, SEXP)])
  
  DT[ISO3 == "0-39" | ISO3 == "40-99", COBI := "<100"]  
  
  DT[ISO3 == "100-149" | ISO3 == "150-199" | ISO3 == "200+", COBI := "100+"]  
  
  DT[AGERP > 110, AGERP := 110]
  
  mid <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], 
                Rate, on = .(aaa = AGERP, Sex = SEXP,
                             ysa = ST, cobi = COBI)]
  
  low <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], 
                LUI, on = .(aaa = AGERP, Sex = SEXP,
                            ysa = ST, cobi = COBI)]
  
  high <- RRates[DT[, .(AGERP, SEXP, COBI, ST = year - YARP)], 
                 UUI, on = .(aaa = AGERP, Sex = SEXP,
                             ysa = ST, cobi = COBI)]
  
  set.seed(set.seed.number[simnumber])
  
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))
  
  # Knocking everyone off at 120 years of age, so I need to adjust RR to zero at 120
  
  ifelse(DT[, AGEP] > kill.off.above, 0, unlist(lapply(vector, rpertfunction))
         
  )
  
} 

# Look up treatr value (it's treatment dependent)

Get.TREATR <- function(S, treat) {
  
  as.numeric(treatment.dt[treatment == treat, ..S])
  
}

# Look up treatment costs (it's treatment dependent)
Get.TREATC <- function(S, treat) {
  
  as.numeric(treatmentcost.dt[treatment == treat & practitioner == "spec", ..S]) * prop.spec +
    as.numeric(treatmentcost.dt[treatment == treat & practitioner == "gp", ..S]) * (1 - prop.spec)
  
}

# Reactivation rate adjustment for existing TB control
Get.RRADJ <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGERP, YARP)])
  
  DT[AGERP > 110, AGERP := 110]
  
  mid <- rradjrates[DT[, .(AGERP, ST = year - YARP)], 
             rate, on = .(aaa = AGERP, ysa = ST)]
  
  low <- rradjrates[DT[, .(AGERP, ST = year - YARP)], 
             lower, on = .(aaa = AGERP, ysa = ST)]
  
  high <- rradjrates[DT[, .(AGERP, ST = year - YARP)], 
             upper, on = .(aaa = AGERP, ysa = ST)]
  
  set.seed(set.seed.number[simnumber])
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))

  unlist(lapply(vector, rpertfunction))
  
}

# Get.EMIGRATE
emigrate.rate <- readRDS("Data/emigrate.rate.rds") # BASELINE assumed rate incorporating both temp and permanent residents 
# emigrate.rate <- readRDS("Data/emigrate.rate.perm.rds") # LOWER assumed rate among permanent residents
emigrate.rate <- as.data.table(emigrate.rate)

Get.EMIGRATE <- function(xDT, year) {
  
  DT <- copy(xDT[, .(year, AGEP, YARP)])
  
  DT[AGEP > 110, AGEP := 110]
  
  # Knocking everyone off at 80 years of age
  emigrate.rate[Age > kill.off.above, Rate := 0]
  
  mid <- emigrate.rate[DT[, .(AGEP)], 
                       Rate, on = .(Age = AGEP)]
  low <- emigrate.rate[DT[, .(AGEP)],
                       lower, on = .(Age = AGEP)]
  high <- emigrate.rate[DT[, .(AGEP)],
                        upper, on = .(Age = AGEP)]
  set.seed(set.seed.number[simnumber])
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))
  unlist(lapply(vector, rpertfunction))
  
  # betaparam <- findbeta2(mid, low, high)
  # rbeta(1, betaparam[1], betaparam[2])
}

# Get.MR
# Look up the mortality rate from vic.mortality
Get.MR <- function(xDT, year, rate.assumption = "Med") {
  
  DT <- copy(xDT[, .(AGEP, SEXP)])
  
  # To lookup all ages beyond 110
  DT[AGEP > 100, AGEP := 100]
  
  # Knocking everyone off at 100 years of age
  vic.mortality[Age > kill.off.above, Prob := 1]
  
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
                          lower, on = .(age = AGEP, sex = SEXP)]
  high <- vic.tb.mortality[DT[, .(AGEP, SEXP)],
                           upper, on = .(age = AGEP, sex = SEXP)]
  set.seed(set.seed.number[simnumber])
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))
  unlist(lapply(vector, rpertfunction))
  
  # betaparam <- findbeta2(mid, low, high)
  # out <- rbeta(1, betaparam[1], betaparam[2])
  # return(out)
  
}

# Get.SAE
# Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAE <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGEP)])
  
  DT[AGEP > 110, AGEP := 110]
  
  mid <- sae.rate[treatment == treat][DT, Rate, on = .(Age = AGEP)]
  low <- sae.rate[treatment == treat][DT, low, on = .(Age = AGEP)]
  high <- sae.rate[treatment == treat][DT, high, on = .(Age = AGEP)]
  
  set.seed(set.seed.number[simnumber])
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))
  
  unlist(lapply(vector, rpertfunction))
  
  # betaparam <- findbeta2(mid, low, high)
  # out <- rbeta(1, betaparam[1], betaparam[2])
  # return(out)
}

# Look up SAE rate from sae.rate (age and treatment dependent)
Get.SAEMR <- function(xDT, treat) {
  
  DT <- copy(xDT[, .(AGEP)])
  
  DT[AGEP > 110, AGEP := 110]
  
  # Knocking everyone off at 100 years of age
  sae.mortality[Age > kill.off.above, Rate := 0]
  
  mid <- sae.mortality[treatment == treat][DT, Rate, on = .(Age = AGEP)]
  low <- sae.mortality[treatment == treat][DT, low, on = .(Age = AGEP)]
  high <- sae.mortality[treatment == treat][DT, high, on = .(Age = AGEP)]
  
  set.seed(set.seed.number[simnumber])
  rpertfunction <- function(x) {
    rpert(1, min = low[x], mode = mid[x], max = high[x])
  }
  vector <- c(1:length(mid))
  unlist(lapply(vector, rpertfunction))
  
}


# MODEL SET UP
# Reading in the data files required for the model run

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
# SAE rate from: ...
sae.mortality <- readRDS("Data/sae.mortality.rds") # this is also required
sae.mortality <- as.data.table(sae.mortality)
# SAE mortality data from: ...
rradjrates <- readRDS("Data/rradjrates.rds")
RRatesbase <- readRDS("Data/RRates.for.psa.rds")

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


unevaluated.flow.cost <- lazy(c(0, 0, param$TESTC, param$TESTC + param$ATTENDCOST, param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST, param$TESTC + param$ATTENDCOST + param$TREATC,
                                param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST + param$SAECOST, 0,
                                0,
                                0, 0, param$TESTC, param$TESTC + param$ATTENDCOST, param$TESTC + param$ATTENDCOST + param$PARTIALTREATCOST, param$TESTC + param$ATTENDCOST + param$TREATC,
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
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               # TBMR = 0.001,
                               RRADJUST = Get.RRADJ(DT, year),
                               # RRADJUST takes into account the fact that a proportion (10% in Victoria)
                               # of TB cases are picked up each year with existing TB control strategies, i.e.
                               # during follow-up as a result of an abnormal CXR during pre-migration off-shore screening.
                               BEGINTREAT = begintrt,
                               # Yet to finalise this parameter. i need to Work out if the chance of beginning treatment 
                               # is age-dependent. May well depend on treatment as well??
                               # TBFOLLOWUPADJUST = Get.TBFOLLOWUPADJUST(DT, year, treatment),
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
                               TREATCOMPLETE = Get.TREAT(S = "treat.complete", treatment),
                               TREATR = Get.TREATR(S = "rate", treatment),
                               TREATC = Get.TREATC(S = "cost.primary", treatment),
                               POP = Get.POP(DT, strategy),
                               UTILITY = Get.UTILITY(treatment),
                               #ATTENDCOST = Get.ATTENDC(DT, S = "cost.attend"),
                               ATTENDCOST = cattend,
                               PARTIALTREATCOST = Get.TREATC(S = "cost.partial", treatment),
                               TBCOST = ctb,
                               SAECOST = csae
)

# To run the model each of the parameters needs
# to be in the environment, i.e. not defined within
# another function so the following loop takes each row of 
# parameter values from the simdata table and runs the model

# I need to place the results from the 
# model runs into a table... here it is:
simrun.output <- c(seq(1, Num_SIm))
simrun.output <- as.data.frame(simrun.output)
simrun.output <- as.data.table(simrun.output)

# this loops over the number of simulations
# and runs the model
for(i in 1:Num_SIm) {
  simnumber <- i
  PSA <- 1
  # Sort out the LTBI prevalence and reacitvation rate data to use
  # Sample the percentile from the LTBI prevalence data:
  set.seed(set.seed.number[simnumber])
  sampled.percentile <- round(rpert(1, min = 25, mode = 50, max = 75))
  sampled.percentile <- paste0(sampled.percentile, "%")
  columns <- c("YARP", "ISO3", "CNSY",
               "SEXP", "AGEP", "NUMP",
               "YOBP", sampled.percentile)
  aust <- copy(austbase[,..columns])
  colpercent <- which(colnames(aust) == sampled.percentile)
  setnames(aust, colpercent, "LTBP")
  # Uses aust.rds file to create a sample input
  pop.master <- CreatePopulationMaster()
  # pop.master <- subset(pop.master, AGERP == 20 & ISO3 == "200+")
  # Select the reactivation rate data to use:
  ratecol <- paste0("rate", sampled.percentile)
  lowercol <- paste0("lower", sampled.percentile)
  uppercol <- paste0("upper", sampled.percentile)
  columns <- c("cobi", "Sex", "aaa", "ysa", 
               ratecol, lowercol, uppercol)
  RRates <- copy(RRatesbase[,..columns])
  colrates <- which(colnames(RRates) == ratecol)
  collower <- which(colnames(RRates) == lowercol)
  colupper <- which(colnames(RRates) == uppercol)
  setnames(RRates, colrates, "Rate")
  setnames(RRates, collower, "LUI")
  setnames(RRates, colupper, "UUI")

  begintrt <- simdata[simnumber, begintrt]
  att <- simdata[simnumber, att]
  attscreen <- simdata[simnumber, attscreen]
  cattend <- simdata[simnumber, cattend]
  csae <- simdata[simnumber, csae]
  cscreenqft <- simdata[simnumber, cscreenqft]
  cscreentst <- simdata[simnumber, cscreentst]
  ctb <- simdata[simnumber, ctb]
  ctreat3HP <- simdata[simnumber, ctreat3HP]
  ctreat4R <- simdata[simnumber, ctreat4R]
  ctreat6H <- simdata[simnumber, ctreat6H]
  ctreat9H <- simdata[simnumber, ctreat9H]
  cparttreat3HP <- simdata[simnumber, cparttreat3HP]
  cparttreat4R <- simdata[simnumber, cparttreat4R]
  cparttreat6H <- simdata[simnumber, cparttreat6H]
  cparttreat9H <- simdata[simnumber, cparttreat9H]
  ctreatspec3HP <- simdata[simnumber, ctreatspec3HP]
  cparttreatspec3HP <- simdata[simnumber, cparttreatspec3HP]
  ctreatspec4R <- simdata[simnumber, ctreatspec4R]
  cparttreatspec4R <- simdata[simnumber, cparttreatspec4R]
  ctreatspec6H <- simdata[simnumber, ctreatspec6H]
  cparttreatspec6H <- simdata[simnumber, cparttreatspec6H]
  ctreatspec9H <- simdata[simnumber, ctreatspec9H]
  cparttreatspec9H <- simdata[simnumber, cparttreatspec9H]
  snqftgit <- simdata[simnumber, snqftgit]
  sntst10 <- simdata[simnumber, sntst10]
  sntst15 <- simdata[simnumber, sntst15]
  spqftgit <- simdata[simnumber, spqftgit]
  sptst10 <- simdata[simnumber, sptst10]
  sptst15 <- simdata[simnumber, sptst15]
  treat.complete.3HP <- simdata[simnumber, treat.complete.3HP]
  treat.complete.4R <- simdata[simnumber, treat.complete.4R]
  treat.complete.6H <- simdata[simnumber, treat.complete.6H]
  treat.complete.9H <- simdata[simnumber, treat.complete.9H]
  treatr3HP <- simdata[simnumber, treatr3HP]
  treatr4R <- simdata[simnumber, treatr4R]
  treatr6H <- simdata[simnumber, treatr6H]
  treatr9H <- simdata[simnumber, treatr9H]
  ttt3HP <- simdata[simnumber, ttt3HP]
  ttt4R <- simdata[simnumber, ttt4R]
  ttt6H <- simdata[simnumber, ttt6H]
  ttt9H <- simdata[simnumber, ttt9H]
  prop.spec <- simdata[simnumber, prop.spec]
  uactivetb <- simdata[simnumber, uactivetb]
  uactivetbr <- simdata[simnumber, uactivetbr]
  uhealthy <- simdata[simnumber, uhealthy]
  ultbi3HP <- simdata[simnumber, ultbi3HP]
  ultbi4R <- simdata[simnumber, ultbi4R]
  ultbi6H <- simdata[simnumber, ultbi6H]
  ultbi9H <- simdata[simnumber, ultbi9H]
  ultbipart3HP <- simdata[simnumber, ultbipart3HP]
  ultbipart4R <- simdata[simnumber, ultbipart4R]
  ultbipart6H <- simdata[simnumber, ultbipart6H]
  ultbipart9H <- simdata[simnumber, ultbipart9H]
  ultbitreatsae <- simdata[simnumber, ultbitreatsae]
  # Create a sample data table of test sensitivity & specificity
  tests.dt <- data.table(tests = c("QTFGIT", "TST10", "TST15"), 
                         SN = c(snqftgit, sntst10, sntst15),
                         SP = c(spqftgit, sptst10, sptst15),
                         # Sensitivity and specificity values from: Abubakar I, Drobniewski F, Southern J, et al. Prognostic value 
                         # of interferon-gamma release assays and tuberculin skin test in predicting the development of active 
                         # tuberculosis (UK PREDICT TB): a prospective cohort study. Lancet Infect Dis 2018; 18(10): 1077-87.
                         # cost.primary = c(74.34, 70.40, 70.40))
                         cost.primary = c(cscreenqft, cscreentst, cscreentst))
  # the line above reflects the fact that the costs of offshore screening are born by the migrant, not
  # Australia's health system
  
  # Create a sample treatment data table
  treatment.dt <- data.table(treatment = c("3HP","4R", "6H", "9H"),
                             treat.complete = c(treat.complete.3HP, treat.complete.4R, treat.complete.6H, treat.complete.9H),
                             rate = c(treatr3HP, treatr4R, treatr6H, treatr9H))
  
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
                                                  cparttreatspec6H, cparttreatspec9H))
  
  # This data table indicates when those who receive LTBI treatment in the first 
  # year after migration are likely to have received that treatment (as an annual proportion).
  timetotreat.dt <- data.table(treatment = c("3HP", "4R", "6H", "9H"),
                               yearfraction = c(ttt3HP, ttt4R, ttt6H, ttt9H))
  # could talk to Michael Flynn to establish how long it takes to complete treatment
  
  # Create a sample utility data table
  # TODO: fix hard coded data table. It should take state.names and create the columns.
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
                                                    ultbitreatsae, 0,
                                                    uhealthy,
                                                    uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
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

  base <- DoRunModel(S0_12, start.year, cycles)
  base <- unlist(base)
  simrun.output[simnumber, basecost := base[1]]
  simrun.output[simnumber, baseqaly := base[2]]

  strat <- DoRunModel(S2, start.year, cycles)
  strat <- unlist(strat)
  simrun.output[simnumber, stratcost := strat[1]]
  simrun.output[simnumber, stratqaly := strat[2]]

}


# Do parallel trick. This requires the doParallel and foreach package
# and makes the loop below run simulaneoulsy in the 4 cores of the 
# computer, and so run four times as quickly!

# my.cl <- makeCluster(4)
# registerDoParallel(my.cl)
# foreach (i = 1:nrow(simdata)) %dopar% {
#   library(data.table)
#   simnumber <- i
#   PSA <- 1
#   begintrt <- simdata[simnumber, begintrt]
#   att <- simdata[simnumber, att]
#   attscreen <- simdata[simnumber, attscreen]
#   cattend <- simdata[simnumber, cattend]
#   csae <- simdata[simnumber, csae]
#   cscreenqft <- simdata[simnumber, cscreenqft]
#   cscreentst <- simdata[simnumber, cscreentst]
#   ctb <- simdata[simnumber, ctb]
#   ctreat3HP <- simdata[simnumber, ctreat3HP]
#   ctreat4R <- simdata[simnumber, ctreat4R]
#   ctreat6H <- simdata[simnumber, ctreat6H]
#   ctreat9H <- simdata[simnumber, ctreat9H]
#   cparttreat3HP <- simdata[simnumber, cparttreat3HP]
#   cparttreat4R <- simdata[simnumber, cparttreat4R]
#   cparttreat6H <- simdata[simnumber, cparttreat6H]
#   cparttreat9H <- simdata[simnumber, cparttreat9H]
#   ctreatspec3HP <- simdata[simnumber, ctreatspec3HP]
#   cparttreatspec3HP <- simdata[simnumber, cparttreatspec3HP]
#   ctreatspec4R <- simdata[simnumber, ctreatspec4R]
#   cparttreatspec4R <- simdata[simnumber, cparttreatspec4R]
#   ctreatspec6H <- simdata[simnumber, ctreatspec6H]
#   cparttreatspec6H <- simdata[simnumber, cparttreatspec6H]
#   ctreatspec9H <- simdata[simnumber, ctreatspec9H]
#   cparttreatspec9H <- simdata[simnumber, cparttreatspec9H]
#   snqftgit <- simdata[simnumber, snqftgit]
#   sntst10 <- simdata[simnumber, sntst10]
#   sntst15 <- simdata[simnumber, sntst15]
#   spqftgit <- simdata[simnumber, spqftgit]
#   sptst10 <- simdata[simnumber, sptst10]
#   sptst15 <- simdata[simnumber, sptst15]
#   treat.complete.3HP <- simdata[simnumber, treat.complete.3HP]
#   treat.complete.4R <- simdata[simnumber, treat.complete.4R]
#   treat.complete.6H <- simdata[simnumber, treat.complete.6H]
#   treat.complete.9H <- simdata[simnumber, treat.complete.9H]
#   treatr3HP <- simdata[simnumber, treatr3HP]
#   treatr4R <- simdata[simnumber, treatr4R]
#   treatr6H <- simdata[simnumber, treatr6H]
#   treatr9H <- simdata[simnumber, treatr9H]
#   ttt3HP <- simdata[simnumber, ttt3HP]
#   ttt4R <- simdata[simnumber, ttt4R]
#   ttt6H <- simdata[simnumber, ttt6H]
#   ttt9H <- simdata[simnumber, ttt9H]
#   prop.spec <- simdata[simnumber, prop.spec]
#   uactivetb <- simdata[simnumber, uactivetb]
#   uactivetbr <- simdata[simnumber, uactivetbr]
#   uhealthy <- simdata[simnumber, uhealthy]
#   ultbi3HP <- simdata[simnumber, ultbi3HP]
#   ultbi4R <- simdata[simnumber, ultbi4R]
#   ultbi6H <- simdata[simnumber, ultbi6H]
#   ultbi9H <- simdata[simnumber, ultbi9H]
#   ultbipart3HP <- simdata[simnumber, ultbipart3HP]
#   ultbipart4R <- simdata[simnumber, ultbipart4R]
#   ultbipart6H <- simdata[simnumber, ultbipart6H]
#   ultbipart9H <- simdata[simnumber, ultbipart9H]
#   ultbitreatsae <- simdata[simnumber, ultbitreatsae]
#   # Create a sample data table of test sensitivity & specificity
#   tests.dt <- data.table(tests = c("QTFGIT", "TST10", "TST15"), 
#                          SN = c(snqftgit, sntst10, sntst15),
#                          SP = c(spqftgit, sptst10, sptst15),
#                          # Sensitivity and specificity values from: Abubakar I, Drobniewski F, Southern J, et al. Prognostic value 
#                          # of interferon-gamma release assays and tuberculin skin test in predicting the development of active 
#                          # tuberculosis (UK PREDICT TB): a prospective cohort study. Lancet Infect Dis 2018; 18(10): 1077-87.
#                          # cost.primary = c(74.34, 70.40, 70.40))
#                          cost.primary = c(cscreenqft, cscreentst, cscreentst))
#   # the line above reflects the fact that the costs of offshore screening are born by the migrant, not
#   # Australia's health system
#   
#   # Create a sample treatment data table
#   treatment.dt <- data.table(treatment = c("3HP","4R", "6H", "9H"),
#                              treat.complete = c(treat.complete.3HP, treat.complete.4R, treat.complete.6H, treat.complete.9H),
#                              rate = c(treatr3HP, treatr4R, treatr6H, treatr9H))
#   
#   # Create a sample treatment cost data table
#   treatmentcost.dt <- data.table(treatment = c("3HP","4R", "6H", "9H", "3HP","4R", "6H", "9H"),
#                                  practitioner = c("gp","gp", "gp", "gp", 
#                                                   "spec","spec", "spec", "spec"),
#                                  cost.primary = c(ctreat3HP, ctreat4R, ctreat6H, ctreat9H,
#                                                   ctreatspec3HP, ctreatspec4R, ctreatspec6H, 
#                                                   ctreatspec9H),
#                                  cost.partial = c(cparttreat3HP, cparttreat4R,
#                                                   cparttreat6H, cparttreat9H,
#                                                   cparttreatspec3HP, cparttreatspec4R,
#                                                   cparttreatspec6H, cparttreatspec9H))
#   
#   # This data table indicates when those who receive LTBI treatment in the first 
#   # year after migration are likely to have received that treatment (as an annual proportion).
#   timetotreat.dt <- data.table(treatment = c("3HP", "4R", "6H", "9H"),
#                                yearfraction = c(ttt3HP, ttt4R, ttt6H, ttt9H))
#   # could talk to Michael Flynn to establish how long it takes to complete treatment
#   
#   # Create a sample utility data table
#   # TODO: fix hard coded data table. It should take state.names and create the columns.
#   utility.dt <- data.table(treatment = c("", "3HP", "4R", "6H", "9H"))
#   utility.dt[, c(state.names) := as.numeric(NA)]
#   
#   utility.dt[treatment == "3HP", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart3HP, ultbi3HP,
#                                                      ultbitreatsae, 0,
#                                                      uhealthy,
#                                                      uhealthy, uhealthy, uhealthy, uhealthy, ultbipart3HP, ultbi3HP,
#                                                      ultbitreatsae, 0,
#                                                      uhealthy, uhealthy,
#                                                      uactivetb, uactivetbr, 0, 0, 0)]
#   
#   utility.dt[treatment == "4R", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy,
#                                                     uhealthy, uhealthy, uhealthy, uhealthy, ultbipart4R, ultbi4R,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy, uhealthy,
#                                                     uactivetb, uactivetbr, 0, 0, 0)]
#   
#   utility.dt[treatment == "6H", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart6H, ultbi6H,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy,
#                                                     uhealthy, uhealthy, uhealthy, uhealthy, ultbipart6H, ultbi6H,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy, uhealthy,
#                                                     uactivetb, uactivetbr, 0, 0, 0)]
#   
#   utility.dt[treatment == "9H", c(state.names) := .(uhealthy, uhealthy, uhealthy, uhealthy, ultbipart9H, ultbi9H,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy,
#                                                     uhealthy, uhealthy, uhealthy, uhealthy, ultbipart9H, ultbi9H,
#                                                     ultbitreatsae, 0,
#                                                     uhealthy, uhealthy,
#                                                     uactivetb, uactivetbr, 0, 0, 0)]
#   
#   utility.dt[treatment == "", c(state.names) := .(uhealthy, uhealthy, NA, NA, NA, NA,
#                                                   NA, NA, 
#                                                   uhealthy,
#                                                   uhealthy, uhealthy, NA, NA, NA, NA,
#                                                   NA, NA,
#                                                   uhealthy, NA,
#                                                   uactivetb, uactivetbr, 0, 0, 0)]
#   
#   base <- DoRunModel(S0_12, start.year, cycles)
#   base <- unlist(base)
#   simrun.output[simnumber, basecost := base[1]]
#   simrun.output[simnumber, baseqaly := base[2]]
#   
#   strat <- DoRunModel(S2, start.year, cycles)
#   strat <- unlist(strat)
#   simrun.output[simnumber, stratcost := strat[1]]
#   simrun.output[simnumber, stratqaly := strat[2]]
# }
# stopCluster(my.cl)

















# Plot of PSA results on cost effectiveness plane.
# The code below will plot the 10,000 model run outputs on a
# cost effectiveness plane.
# A blue willingness to pay line is drawn on the plane too
# and the colour of the simulations will be either green or red
# depending on whether the ICER value is under or over the WTP.

WTP = 50000 # willingness to pay threshold

plotdata <- cbind(simdata, simrun.output)
plotdata[, incremental.qaly := stratqaly - baseqaly] 
plotdata[, incremental.cost := stratcost - basecost] 
plotdata[, icer := (stratcost - basecost)/(stratqaly - baseqaly)]
plotdata[, dominated := ifelse(incremental.qaly < 0 & incremental.cost > 0, 1, NA)] 
plotdata[, dominant := ifelse(incremental.qaly > 0 & incremental.cost < 0, 1, NA)]
plotdata <- plotdata[, c("incremental.cost", "incremental.qaly", "icer",
                         "dominated", "dominant" )]

plotdata[icer > WTP, wtp.colour := 0] 
plotdata[icer < WTP, wtp.colour := 1]
plotdata[dominated == 1, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer < WTP, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer > WTP, wtp.colour := 1]


# Save this table to file

if (onshore == 1) {
  
  saveRDS(simdata, "Data/PSA/onshore.rds")
  
} else if (onshore == 0) {

  saveRDS(simdata, "Data/PSA/offshore.rds")
  
}


# # Read back in simdata 
# simdata <- readRDS("Data/PSA/simdata.rds")


widthcm <- 8
heightcm <- 6


if (onshore == 1) {
  
  plotdata[, wtp.colour := as.factor(wtp.colour)]
  
  # ONSHORE
  ylimmin <- -1
  ylimmax <- 7
  xlimmin <- -100
  xlimmax <- 100
  
  pointsize <- 2.5
  textsize <- 6
  textsize2 <- 20

  
  myplot1<-
    ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000,
                       colour = wtp.colour)) +
    geom_point(size = pointsize, alpha = 1, na.rm = T) +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_abline(intercept = 0, slope = (50000/1000000)/1,
                colour = "gray85",
                size = 1) +
    # geom_abline(intercept = 0, slope = (100000/1000000)/1,
    #             colour = "gray55",
    #             size = 1) +
    geom_abline(intercept = 0, slope = (200000/1000000)/1,
                colour = "gray25",
                size = 1) +
    labs(x = "Incremental QALYs", 
         y = "Incremental cost (AUD$millions)") +
    scale_colour_manual(values = c("black", "black")) +
    scale_y_continuous(breaks = seq(-500, 250, 1)) +
    scale_x_continuous(breaks = seq(-5000000, 10000000, 20)) +
    theme_bw() +
    coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
    theme(text = element_text(size = textsize2),
          panel.border = element_blank(),
          legend.position = "none")
  

    tiff('Figures/psa onshore.tiff',
         units = "in", width = widthcm, height = heightcm,
         res = 200)
    print(myplot1)
    dev.off()

} else if (onshore == 0) {
  
  ylimmin <- -2
  ylimmax <- 6
  xlimmin <- -180
  xlimmax <- 120
  
  pointsize <- 2.5
  textsize <- 6
  textsize2 <- 20
  
  # OFFSHORE
  myplot1<- 
    ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000)) +
    geom_point(size = pointsize, alpha = 1, na.rm = T) +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_abline(intercept = 0, slope = (50000/1000000)/1,
                colour = "gray85",
                size = 1) +
    # geom_abline(intercept = 0, slope = (100000/1000000)/1,
    #             colour = "gray55",
    #             size = 1) +
    geom_abline(intercept = 0, slope = (200000/1000000)/1,
                colour = "gray25",
                size = 1) +
    labs(x = "Incremental QALYs", 
         y = "Incremental cost (AUD$millions)") +
    #scale_colour_manual(values = c("black", "black")) +
    scale_y_continuous(breaks = seq(-500, 250, 1)) +
    scale_x_continuous(breaks = seq(-5000000, 10000000, 20)) +
    theme_bw() +
    coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
    theme(text = element_text(size = textsize2),
          panel.border = element_blank(),
          legend.position = "none")
  
    tiff('Figures/psa offshore.tiff',
         units = "in", width = widthcm, height = heightcm,
         res = 200)
    print(myplot1)
    dev.off()

}

# Plot of acceptability curve
# work out the proportion cost-effective
plotdata[, cheaper.and.worse := ifelse(incremental.qaly < 0 & incremental.cost < 0, 1, NA)]
cheaper.and.worse.list <- plotdata$icer[plotdata$cheaper.and.worse == 1]
plotdata[, better.and.costly := ifelse(incremental.qaly > 0 & incremental.cost > 0, 1, NA)]
better.and.costly.list <- plotdata$icer[plotdata$better.and.costly == 1]
dominant <- plotdata$dominant
dominated <- plotdata$dominated

maxwtp <- 300000
wtp <- c(0:maxwtp)


# wtp <- 473
# sum(dominant == 1, na.rm = TRUE)
# sum(better.and.costly.list <= wtp, na.rm = TRUE) 
# sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)
# length(cheaper.and.worse.list)

propcosteffectivefunc <- function(wtp) {
  ((sum(dominant == 1, na.rm = TRUE) + 
         sum(better.and.costly.list <= wtp, na.rm = TRUE) + 
      sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)) / length(cheaper.and.worse.list)) * 100
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
    coord_cartesian(xlim = c(0, maxwtp), ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_continuous(label = comma, breaks = seq(0, 300000, 50000)) +
  theme_bw() +
  theme(text = element_text(size = 25))


if (onshore == 1) {
  tiff('Figures/acceptability onshore.tiff',
       units = "in", width = widthcm, height = heightcm,
       res = 200)
  print(myplot1)
  dev.off()
} else if (onshore == 0) {
  tiff('Figures/acceptability offshore.tiff',
       units = "in", width = widthcm, height = heightcm,
       res = 200)
  print(myplot1)
  dev.off()
}