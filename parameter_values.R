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
# source("Medical costs.R")