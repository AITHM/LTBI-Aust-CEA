

# This is the file that analyses the output data from the cost-effectiveness analysis.
# The file CB-TLTBI.R needs to be run to create the output that this script then analyses.

# Creation of Table 2 - population numbers, costs and cost effectiveness of various treatment strategies.

library(tidyverse)
library(data.table)
library(plyr)

# Read in the output files
filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern="*.rds", full.names=TRUE)
files <- lapply(filenames, readRDS)

# Create a list of the names of the output files
namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", pattern="*.rds")
namelist <- gsub("\\b.rds\\b","", namelist)
namelist <- gsub("\\bS2\\b","", namelist)
namelist <- substring(namelist, 2)

# Name the files in the list
files <- setNames(files, namelist)

# Create a column with the YEAR and strategy name within each data table
counter <- 0
files<-lapply(files, function(dt) {
  dt <- as.data.table(dt)
  dt[, YEAR :=  cycle + 2020]
  counter <<- counter + 1
  dt[, STRAT := namelist[counter]]
})

# Define some model parameters that might be used below
  # Time horizon
startyear <- 2020 
finalyear <- 2050
cycleyears <- finalyear - startyear

  # Target population
targetfunc <- function(dt) {
  dt <- subset(dt, ISO3 == "150+"|ISO3 == "100-149" & AGERP > 10 & AGERP < 35)
  dt
}

# Finding the baseline quantities
base <- files[[1]]
dt <- files[[5]]

# total baseline cost
a <- which( colnames(base) == "SC.p.sus" )
b <- which( colnames(base) == "SC.p.emigrate" )
base$SCsum <- rowSums(base[, a:b], na.rm = TRUE)
a <- which( colnames(base) == "FC.p.sus" )
b <- which( colnames(base) == "FC.p.emigrate" )
base$FCsum <- rowSums(base[, a:b], na.rm =TRUE)
totbasecost <- sum(base$SCsum) + sum(base$FCsum)

# total baseline QALYS
a <- which( colnames(base) == "SQ.p.sus" )
b <- which( colnames(base) == "SQ.p.emigrate" )
base$SQsum <- rowSums(base[, a:b], na.rm =TRUE)
qalybase <- sum(base$SQsum)

# total baseline tb cases
basetbcount <- base[, sum(p.tb)]

tabfunc<-function(dt) {
  
  # Add the name of the strategy
  nameofdt <- dt$STRAT[1]
  
  # annual migrant inflow
  migflow <- dt[YEAR == 2020 & YARP == 2020, sum(NUMP),]
  
  # annual average emigration inflow
  emigflow <- dt[, sum(p.emigrate)]/cycleyears
  
  emigflow <- dt[YEAR == 2022 & YARP == 2020, sum(p.emigrate)]
  
  # number screened/tested annually
  # targetgroup <- targetfunc (dt)
  # numscreened <- targetgroup[YEAR == 2020 & YARP == 2020, sum(NUMP),]
  cdt <- copy(dt)
  numscreened <- cdt[YEAR == 2021 & YARP == 2020, sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
                  sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
                  sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # number referred annually
  # find name of screening test
  testing <- dt$Test[1]
  # create a lookup table
  tests.dt <- data.table(tests = c("QTFGIT", "TST05", "TST10", "TST15", ""), SN = c(0.6104, 0.74, 0.7532, 0.6753, 1),
                         SP = c(0.7784, 0.56, 0.6679, 0.7726, 1))
  TESTSN <- tests.dt[tests == testing, SN]
  TESTSP <- tests.dt[tests == testing, SP]
  targetgroup <- targetfunc (dt)
  numref <- targetgroup[YEAR == 2020 & YARP == 2020, sum(LTBP),] * TESTSN + targetgroup[YEAR == 2020 & YARP == 2020, sum(NUMP) - sum(LTBP),] * (1 - TESTSP)
  
  # number attending annually (should be 0.836 * the number referred)
  numatt <- dt[YEAR == startyear+1 & YARP == 2020, sum(p.sus.nbt) + sum(p.sus.nct) +
       sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
       sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number screened during the whole time period (just multiply the annual number by ten?)
      # Method 1
      # testing <- dt$Test[1]
      # create a lookup table
      # tests.dt <- data.table(tests = c("QTFGIT", "TST05", "TST10", "TST15"), SN = c(0.6104, 0.74, 0.7532, 0.6753),
      #                        SP = c(0.7784, 0.56, 0.6679, 0.7726))
      # TESTSN <- tests.dt[tests == testing, SN]
      # TESTSP <- tests.dt[tests == testing, SP]
      # targetgroup <- targetfunc (dt)
      # totscreen <- targetgroup[YEAR == YARP, sum(LTBP),] * TESTSN + targetgroup[YEAR == YARP , sum(NUMP) - sum(LTBP),] * (1 - TESTSP)
      # Method two
      # cdt <- copy(dt)
      # cdt <- as.data.table(cdt)
      # totscreen <- cdt[YEAR == YARP + 1, sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
      #                 sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
      #                 sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  #totscreen <- numscreened * cycleyears
  
  cdt <- copy(dt)
  totscreen <- cdt[, sum(p.sus.nf) + sum(p.sus.nbt) + sum(p.sus.nct) +
                       sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nf) + sum(p.ltbi.nbt) +
                       sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # total number attended during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  totatt <- cdt[YEAR == YARP + 1, sum(p.sus.nbt) + sum(p.sus.nct) +
                       sum(p.sus.sae) + sum(p.sus.tc) + sum(p.ltbi.nbt) +
                       sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)]
  
  # total number treated (any treatment) during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  numberstarttreat <- cdt[YEAR == YARP + 1, sum(p.sus.nct) + sum(p.sus.sae) + sum(p.sus.tc) +
                              sum(p.ltbi.nct) + sum(p.ltbi.sae) + sum(p.ltbi.tc)] 
  
  # total number treated (effective) during the whole time period
  cdt <- copy(dt)
  cdt <- as.data.table(cdt)
  cdt[YEAR == YARP + 1, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  numbertreated <- cdt[YEAR == YARP + 1, sum(p.sus.tc) + sum(p.ltbi.tc)] 
  
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
  
  #Cost per TB case prevented
  costpertb <- totcost/tbprev
  
  # number needed to screen (to prevent a tb case)
  targetgroup <- targetfunc (dt)
  numberscreened <- targetgroup[YEAR == startyear, sum(NUMP)] * 10
  nns <- numberscreened/tbprev
  
  
  
  
  # number needed to effectively treat (to prevent a tb case)
  nnt <- numbertreated/tbprev
  
  # number needed to at least start treat (to prevent a tb case)
  nnbt <- numberstarttreat/tbprev
  
  
  
  
  # number of SAEs among those with ltbi
  saeltbi <- dt[, sum(p.ltbi.sae)]
  
  # number of SAEs among those without ltbi
  saesus <- dt[, sum(p.sus.sae)]
  
  # number of SAE deaths among those with ltbi
  saedeathltbi <- dt[, sum(p.ltbi.sae.death)]
  
  # number of SAE deaths among those without ltbi
  saedeathusus <- dt[, sum(p.sus.sae.death)]
  
  # total baseline QALYS
  qalybase
  
  # total number of QALYS
  a <- which( colnames(dt) == "SQ.p.sus" )
  b <- which( colnames(dt) == "SQ.p.emigrate" )
  dt$SQsum <- rowSums(dt[, a:b], na.rm =TRUE)
  qalytot <- sum(dt$SQsum)
  
  #Incremental QALYs
  incremqaly <- qalytot - qalybase
  
  # Cost per QALY - ICER
  costperqaly <- totaddcost/incremqaly
  
  tablist<-list(nameofdt,
                migflow,
                emigflow,
                numscreened,
                numref,
                numatt,
                totscreen,
                totatt,
                numberstarttreat,
                numbertreated,
                totbasecost,
                totcost,
                totaddcost,
                tbtotal,
                tbprev,
                costpertb,
                nns,
                nnt,
                nnbt,
                saeltbi,
                saesus,
                saedeathltbi,
                saedeathusus,
                qalybase,
                qalytot,
                incremqaly,
                costperqaly)
  
  tablist[is.na(tablist)] <- 0
  
  namelist<-c("strategy",
              "annual migrant flow",
              "annual average emigration flow",
              "annual number screened",
              "annual number referred",
              "annual number that attended",
              "total number screened",
              "total number attended",
              "total number beginning treatment",
              "total number treated effectively",
              "total base cost",
              "total cost",
              "total additional cost",
              "total TB cases",
              "TB cases prevented",
              "cost per TB case prevented",
              "Number needed to screen",
              "Number needed to treat",
              "Number needed to begin treatment",
              "number of SAEs in those with ltbi",
              "number of SAEs in those without ltbi",
              "number of SAE deaths among those with ltbi",
              "number of SAE deaths among those without ltbi",
              "Baseline QALY total",
              "Strategy QALYS total",
              "Incremental QALYS",
              "ICER, i.e. Cost per QALY")
  names(tablist) <- namelist
  pop <- as.data.frame(tablist)
  pop
}

# Create the table
table1 <- rbindlist(lapply(files, tabfunc))

# Write the table to clipboard so I can paste it into Excel
write.table(table1, "clipboard", sep="\t", row.names=FALSE)





















#############################RUBBISH###########################################################

base <- files[[1]]
dt <- files[[10]]
unique(dt$STRAT)
# 
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
base[, p.sum := rowSums(.SD), .SDcols = c(9:29)]
base[, sum(p.sum), by = cycle]
base[, sum(p.tb), by = cycle]
base[, sum(NUMP), by = cycle]


dt[YARP == 2020, sum(NUMP), by = cycle]
dt[YEAR == 2020 & YARP == 2020, sum(NUMP), by = cycle]
dt[YARP == 2021, sum(NUMP), by = cycle]
dt[YARP == 2022, sum(NUMP), by = cycle]
dt[,sum(p.emigrate), by = cycle]

check <- subset(dt,YARP == 2020)
check <- subset(check,AGERP == 25)
check <- subset(check,ISO3 == "150+")


bc <- subset(base,YARP == 2020)
bc <- subset(bc,AGERP == 25)
bc <- subset(bc,ISO3 == "150+")


# 
# # Write the table to clipboard so I can paste it into Excel
# write.table(bc, "clipboard", sep="\t", row.names=FALSE)


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
