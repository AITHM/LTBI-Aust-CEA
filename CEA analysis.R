

# This is the file that analyses the output data from the cost-effectiveness analysis.
# The file CB-TLTBI.R needs to be run to create the output that this script then analyses.

# Creation of Table 2 - population numbers, costs and cost effectiveness of various treatment strategies.

library(tidyverse)
library(data.table)


# Model setup located within this file.
# It defines all the states, transition matrices, strategies, costs and parameters.
# setwd("H:/Katie/PhD/ CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
# source("CB-TLTBI Parameter values.R")

# Read in the output files
filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds", full.names = TRUE)
files <- lapply(filenames, readRDS)

# Create a list of the names of the output files
namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern = "*.rds")
namelist <- gsub("\\b.rds\\b", "", namelist)
namelist <- gsub("\\bS2\\b", "", namelist)
namelist <- substring(namelist, 2)

# Name the files in the list
files <- setNames(files, namelist)

# Create a column with the YEAR and strategy name within each data table
counter <- 0
files <- lapply(files, function(dt) {
  dt <- as.data.table(dt)
  dt[, YEAR :=  cycle + start.year]
  counter <<- counter + 1
  dt[, STRAT := namelist[counter]]
})

# Finding the baseline quantities
base <- files[[1]]
dt <- files[[2]]

# total baseline cost
a <- which( colnames(base) == "SC.p.sus" )
b <- which( colnames(base) == "SC.p.emigrate" )
base$SCsum <- rowSums(base[, a:b], na.rm = TRUE)
a <- which( colnames(base) == "FC.p.sus" )
b <- which( colnames(base) == "FC.p.emigrate" )
base$FCsum <- rowSums(base[, a:b], na.rm = TRUE)
totbasecost <- sum(base$SCsum) + sum(base$FCsum)
base[, SCsum:= NULL]
base[, FCsum:= NULL]


# total baseline QALYS
a <- which( colnames(base) == "SQ.p.sus" )
b <- which( colnames(base) == "SQ.p.emigrate" )
base$SQsum <- rowSums(base[, a:b], na.rm = TRUE)
qalybase <- sum(base$SQsum)
base[, SQsum:= NULL]

# total baseline tb cases
basetbcount <- base[, sum(p.tb)]

# total baseline tb cases
basetbdeath <- base[YEAR == final.year, sum(p.tb.death)]

tabfunc <- function(dt) { 
  dt <- as.data.table(dt)
  
  # Add the name of the strategy
  nameofdt <- dt$STRAT[1]
  
  # annual migrant inflow
  migflow <- dt[YEAR == start.year & YARP == start.year, sum(NUMP),] * (finalinflow + 1)
  
  # percentage with LTBI
  percentltbi <- (dt[YEAR == start.year & YARP == start.year,
                      sum(LTBP),]/dt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * 100
  
  # annual average number emigrating
  emigflow <- dt[YEAR == final.year, sum(p.emigrate)]/totalcycles
  
  # probability of emigration over time horizon
  emigpercent <- (dt[YEAR == final.year, sum(p.emigrate)] / migflow) * 100
  
  # one year probability of emigration 
  emigproboneyear <- (-1/totalcycles * log(1 - (emigpercent/100))) * 100
  
  # annual number screened/tested
  # all those in every state except the "no test" states (and ignoring TB cases)
  numscreened <- dt[, sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
                  sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
                  sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numscreened <- (cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * attscreen
  
  # annual number referred 
  # this will either be:
      # those with and without LTBI in the target population (which I get using the targetfunc) 
      # multiplied by the relevent sensitivities and specificities and multiplied by
      # the proportion that attended the screen:
  # cdt <- targetfunc(dt)
  # cdt <- as.data.table(cdt)
  # numref <- ((cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * sntst10) +
  #   ((cdt[YEAR == start.year & YARP == start.year, sum(NUMP),] -
  #      cdt[YEAR == start.year & YARP == start.year, sum(LTBP),]) * attscreen * (1 - sptst10))
  
  # or it will be the number that attended in each of the LTBI and SUS categories
  # divided by the chance of attending
  numref <- dt[, sum(p.sus.nbt) +
                      sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                       sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] / att
  # YEAR == start.year + 1 & YARP == start.year, 
  
  
  # number attending annually
  # all those in the "did not begin treatment", "did not complete treatment" etc.
  numatt <- dt[, sum(p.sus.nbt) + sum(p.sus.nct) +
                 sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 

  # total number screened/tested
  totscreen <- numscreened
  
  # total number attended during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  totatt <- cdt[, sum(p.sus.nbt) + sum(p.sus.nct) +
                       sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                       sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # total number treated (any treatment) during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  numberstarttreat <- cdt[, sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) +
                              sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number that completed treatment during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  numbercomptreated <- (cdt[, sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) +
                            sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]) *
    as.numeric(treatment.dt[treatment == treatmentlist, treat.complete])
  
  # total base cost
  totbasecost
  
  # total cost of strategy
  a <- which( colnames(dt) == "SC.p.sus" )
  b <- which( colnames(dt) == "SC.p.emigrate" )
  dt$SCsum <- rowSums(dt[, a:b], na.rm = TRUE)
  a <- which( colnames(dt) == "FC.p.sus" )
  b <- which( colnames(dt) == "FC.p.emigrate" )
  dt$FCsum <- rowSums(dt[, a:b], na.rm = TRUE)
  sum(dt$FCsum)
  totcost <- sum(dt$SCsum) + sum(dt$FCsum)
  
  # incremental cost of strategy
  totaddcost <- totcost - totbasecost
  
  # total number of tb cases
  tbtotal <- dt[, sum(p.tb)]
  
  # number of tb cases prevented
  tbprev <- basetbcount - tbtotal
  
  # percentage of tb cases prevented
  tbprevpercent <- (tbprev/basetbcount) * 100
  
  # number of tb deaths 
  tbdeath <- dt[YEAR == final.year, sum(p.tb.death)]
  
  # number of tb deaths prevented
  tbdeathprev <- basetbdeath - tbdeath
  
  #Cost per TB death prevented
  if (totaddcost < 0) {
    costpertbdeath <- "cost saving"
  } else {
    costpertbdeath <- totaddcost/tbdeathprev
  }
  
  #Cost per TB case prevented
  if (totaddcost < 0) {
    costpertb <- "cost saving"
  } else {
    costpertb <- totaddcost/tbprev
  }
  
  # number needed to screen (to prevent a tb case)
  nns <- totscreen/tbprev
  
  # number needed to at least start treat (to prevent a tb case)
  nnbt <- numberstarttreat/tbprev
  
  # number needed to complete treatment (to prevent a tb case)
  nnct <- numbercomptreated/tbprev
  
  # number of SAEs among those with ltbi
  saeltbi <- dt[YEAR == YARP + 1, sum(p.ltbi.sae)]
  
  # number of SAEs among those without ltbi
  saesus <- dt[YEAR == YARP + 1, sum(p.sus.sae)]
  
  # number of SAE deaths among those with ltbi
  saedeathltbi <- dt[YEAR == YARP + 2, sum(p.ltbi.sae.death)]
  
  # number of SAE deaths among those without ltbi
  saedeathusus <- dt[YEAR == YARP + 2, sum(p.sus.sae.death)]
  
  # lifetime risk of TB for someone who has a TST10mm
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  pos.screen <- cdt[, (sum(p.sus) * attscreen * (1 - sptst10)) +
                  (sum(p.ltbi) * attscreen * (sntst10))]
  
  # total baseline QALYS
  qalybase
  
  # total number of QALYS
  a <- which( colnames(dt) == "SQ.p.sus" )
  b <- which( colnames(dt) == "SQ.p.emigrate" )
  dt$SQsum <- rowSums(dt[, a:b], na.rm =TRUE)
  qalytot <- sum(dt$SQsum)
  
  #Incremental QALYs per 1000
  incremqaly1000 <- ((qalytot - qalybase)/totscreen)*1000
  
  #Incremental QALYs
  incremqaly <- qalytot - qalybase
  
  # Cost per QALY - ICER
  
  if (totaddcost < 0 & incremqaly > 0) {
    
    description <- "cost saving"
    costperqaly <- totaddcost/incremqaly
    
  } else if (totaddcost > 0 & incremqaly < 0) {
    
    description <- "dominated"
    costperqaly <- totaddcost/incremqaly
    
  } else if (totaddcost < 0 & incremqaly < 0) {
    
    costperqaly <- totaddcost/incremqaly
    description <- "lower cost and QALY"
    
  } else {
    
    costperqaly <- totaddcost/incremqaly
    description <- totaddcost/incremqaly
    
  }
  
  tablist<-list(nameofdt,
                migflow,
                percentltbi,
                emigflow,
                emigpercent,
                emigproboneyear,
                numscreened,
                numref,
                numatt,
                totscreen,
                totatt,
                numberstarttreat,
                numbercomptreated,
                totbasecost,
                totcost,
                totaddcost,
                tbtotal,
                tbprev,
                tbprevpercent,
                tbdeath,
                tbdeathprev,
                costpertbdeath,
                costpertb,
                nns,
                nnbt,
                nnct,
                saeltbi,
                saesus,
                saedeathltbi,
                saedeathusus,
                pos.screen,
                qalybase,
                qalytot,
                incremqaly1000,
                incremqaly,
                costperqaly,
                description)
  
  tablist[is.na(tablist)] <- 0
  
  namelist<-c("strategy",
              "annual migrant flow",
              "percent of cohort with ltbi",
              "annual average number emigrating",
              "Probability of emigration over time horizon",
              "Probability of emigration in one year",
              "annual number screened",
              "annual number referred",
              "annual number that attended",
              "total number screened",
              "total number attended",
              "total number beginning treatment",
              "total number completing treatment",
              "total base cost",
              "total cost",
              "total additional cost",
              "total TB cases",
              "TB cases prevented",
              "Percentage of all TB cases prevented",
              "total TB deaths",
              "total TB deaths prevented",
              "cost per TB death prevented",
              "cost per TB case prevented",
              "Number needed to screen",
              "Number needed to begin treatment",
              "Number needed to complete treatment",
              "number of SAEs in those with ltbi",
              "number of SAEs in those without ltbi",
              "number of SAE deaths among those with ltbi",
              "number of SAE deaths among those without ltbi",
              "number who screened positive",
              "Baseline QALY total",
              "Strategy QALYS total",
              "Incremental QALYS per 1000",
              "Incremental QALYS",
              "ICER, i.e. Cost per QALY",
              "Outcome")
  names(tablist) <- namelist
  pop <- data.frame(tablist)
  pop$cost.per.TB.death.prevented <- as.character(pop$cost.per.TB.death.prevented)
  pop$cost.per.TB.case.prevented <- as.character(pop$cost.per.TB.case.prevented)
  pop$Outcome <- as.character(pop$Outcome)
  pop <- pop[1,]
}

# Create the table
table1 <- rbindlist(lapply(files, tabfunc))


# Write the table to clipboard so I can paste it into Excel
write.table(table1, "clipboard", sep = "\t", row.names = FALSE)

















#############################RUBBISH###########################################################


# 
# 
# # total baseline cost
# a <- which( colnames(base) == "SC.p.sus" )
# b <- which( colnames(base) == "SC.p.emigrate" )
# base$SCsum <- rowSums(base[, a:b], na.rm = TRUE)
# sum(base$SCsum)
# 
# a <- which( colnames(base) == "FC.p.sus" )
# b <- which( colnames(base) == "FC.p.emigrate" )
# base$FCsum <- rowSums(base[, a:b], na.rm =TRUE)
# sum(base$FCsum)
# 
# totbasecost <- sum(base$SCsum) + sum(base$FCsum)
# 
# 
# # total cost of strategy
# a <- which( colnames(dt) == "SC.p.sus" )
# b <- which( colnames(dt) == "SC.p.emigrate" )
# dt$SCsum <- rowSums(dt[, a:b], na.rm = TRUE)
# sum(dt$SCsum)
# 
# a <- which( colnames(dt) == "FC.p.sus" )
# b <- which( colnames(dt) == "FC.p.emigrate" )
# dt$FCsum <- rowSums(dt[, a:b], na.rm = TRUE)
# sum(dt$FCsum)
# 
# totcost <- sum(dt$SCsum) + sum(dt$FCsum)
# 
# 
# dt[, sum(p.tb), by = cycle]
# 
# 
# 
# ggplot(dt, aes(x = cycle,y = ))+
#   geom_bar(stat="identity", position = position_dodge(width = 0.7),
#            colour="black")+
#   labs(x = "Years since migration", 
#        y = "TB rate per 100,000",
#        fill="Gender") +
#   scale_y_continuous(breaks = seq(0, 1500, 10))+
#   theme_bw()+
#   scale_fill_manual(values = c("grey65", "grey88"))+
#   #scale_fill_manual(values = c("salmon1", "skyblue3"))+
#   geom_errorbar(aes(x=ysag, ymin=upper*100000, 
#                     ymax=lower*100000),
#                 width=0.1,position = position_dodge(width = 0.7))+
#   coord_cartesian(ylim = c(0,120))+
#   theme(text = element_text(size=textsize, family="TT Arial"),
#         legend.position = c(0.80, 0.8),
#         axis.text.x = element_text(angle=45, hjust=1),
#         strip.text = element_blank())
# 
# 
# 
# 
# 
# check <- subset(dt, dt$ISO3=="100-149")
# check <- subset(check, cycle>0)
# 
# 
# 
# base <- files[[1]]
# dt <- files[[2]]
# unique(dt$STRAT)
# #
# base[, p.sum := rowSums(.SD), .SDcols = c(9:29)]
# base[, sum(p.sum), by = cycle]
# base[, sum(p.tb), by = cycle]
# base[, sum(NUMP), by = cycle]
# []
# 
# dt[YARP == 2020, sum(NUMP), by = cycle]
# dt[YEAR == 2020 & YARP == 2020, sum(NUMP), by = cycle]
# dt[YARP == 2021, sum(NUMP), by = cycle]
# dt[YARP == 2022, sum(NUMP), by = cycle]
# dt[,sum(p.emigrate), by = cycle]
# wa
# check <- subset(dt, YARP == 2020)
# check <- subset(check, AGERP == 30 )
# check <- subset(check, ISO3 == "150-199")
# 
# 
# bc <- subset(base, YARP == 2020)
# bc <- subset(bc, AGERP == 30 )
# bc <- subset(bc, ISO3 == "150-199")
# 
# 
# # Write the table to clipboard so I can paste it into Excel
# write.table(dt, file = "clipboard-16384", sep = "\t", row.names = FALSE)
# options(scipen = 999)



# # # year when 99.9% dead
# cyclecount <- 120
# for (i in 2:cyclecount) {
#   total <- dt[YEAR == start.year + i & YARP == start.year, sum(NUMP),]
#   out <- dt[YEAR == start.year + i & YARP == start.year, sum(p.death),] +
#     dt[YEAR == start.year + i & YARP == start.year, sum(p.tb.death),] +
#     dt[YEAR == start.year + i & YARP == start.year, sum(p.emigrate),] +
#     dt[YEAR == start.year + i & YARP == start.year, sum(p.ltbi.sae.death),]
#   if (out/total > 0.999) {
#     cycle.when.almost.all.dead <- i
#     break
#   }
# }
# cycle.when.almost.all.dead

# 
# 
# 
# 
# 
# # 
# # 
# # check <- dt[YEAR == 2021 & YARP == 2021,]
# # sum(check$NUMP)
# # 
# # check <- dt[YEAR == 2020 & YARP == 2022,]
# # 
# # check <- dt[YEAR == 2021 & YARP == 2022,]
# # 
# # check <- dt[YEAR == 2022 & YARP == 2022,]
# # 
# # check <- dt[YEAR == 2023 & YARP == 2022,]
# # 
# 
# 
# 
# # What I need. I need to know for each strategy:
# # Method:
# # time horizon;
# # target population, i.e. TB incidence in country of birth and age-group;
# # testing and treatments used;
# # parameter values used;
# # migrant inflows each year
# # dt[YEAR == 2020 & cycle == 0, sum(NUMP)]
# 
# # Results:
# # total additional cost of the strategy, i.e. over baseline?
# # number of migrants requiring follow-up per year;
# # total additional cost of the program per person, i.e. per migrant screened or followed-up??
# # total number referred
# # number of TB cases prevented;
# # number of TB deaths prevented;
# # number needed to screen to prevent a TB case;
# # number needed to treat to prevent a TB case;
# # cost per TB case prevented;
# # cost per TB death prevented;
# # number of SAEs caused;
# # number of SAE deaths caused;
# # total additional QALYS;
# # cost per QALY gained.
# 
