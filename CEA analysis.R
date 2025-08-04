
#'===========================================================================================================
#' This is the file that can analyse the output data from the cost-effectiveness analysis.
#' The file Model run.R needs to be run to create the output files that this script then analyses.
#' Before running Model run.R, define the strategies you want to consider and parameter values
#' in the script "Parameter values".
#' 
#' Inputs:
#' Model run.R needs to be run
#' 
#' Output:
#' This script outputs a table onto the clipboard that includes the results 
#' for each strategy (and its baseline) in separate rows.
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml
#'===========================================================================================================



# Define a relative path from the script directory to the output folder
output_path <- file.path(this_file_path, "Data")

# Read in the .rds files from that folder
filenames <- list.files(output_path, pattern = "\\.rds$", full.names = TRUE)
files <- lapply(filenames, readRDS)


# Create a list of output file names
namelist <- list.files(output_path, pattern = "\\.rds$")

# Clean up the names
namelist <- gsub("\\.rds$", "", namelist)  # Remove file extension
namelist <- gsub("S2", "", namelist)       # Remove S2
namelist <- substring(namelist, 2)         # Remove first character

# Assign cleaned names to the list of files
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
  
  # annual of target group with LTBI
  migflow <- dt[YEAR == start.year & YARP == start.year, sum(NUMP),] * (finalinflow + 1)
  
  # percentage with LTBI
  cdt <- targetfunc(dt)
  cdt <- as.data.table(cdt)
  percentltbi <- (cdt[YEAR == start.year & YARP == start.year,
                      sum(LTBP),]/cdt[YEAR == start.year & YARP == start.year, sum(NUMP),]) * 100
  
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
  
  # annual number referred 
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
  numbercomptreated <- cdt[, sum(p.sus.tc) + sum(p.ltbi.tc)]
  
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