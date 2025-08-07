# Model run.R (Refactored)

library(data.table)
library(lazyeval)
library(readxl)

# Set working directory
this_file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this_file_path)

# Load parameter values and setup
parameters.already.set <- 0
source("parameter_values.R")
source("CB-TLTBI Functions.R")
source("CB-TLTBI_DataPreparation.R")

# Load additional required data from Excel
mortality <- as.data.table(read_excel("parameters.xlsx", sheet = "mortality"))
sae.rate <- as.data.table(read_excel("parameters.xlsx", sheet = "sae_rate"))
sae.mortality <- as.data.table(read_excel("parameters.xlsx", sheet = "sae_mortality"))
rradjrates <- as.data.table(read_excel("parameters.xlsx", sheet = "rradjrates"))
tb.mortality <- as.data.table(read_excel("parameters.xlsx", sheet = "tb_mortality"))

vic.mortality <- mortality
vic.tb.mortality <- tb.mortality
vic.tb.mortality[, age := as.integer(age)]

# Create state names and derived labels
state.names <- c("p.sus", "p.sus.notest", "p.sus.nf", "p.sus.nbt", "p.sus.nct", "p.sus.tc",
                 "p.sus.sae", "p.sus.sae.death", "p.sus.no.risk",
                 "p.ltbi", "p.ltbi.notest", "p.ltbi.nf", "p.ltbi.nbt", "p.ltbi.nct", "p.ltbi.tc",
                 "p.ltbi.sae", "p.ltbi.sae.death", "p.ltbi.ongoing.risk", "p.ltbi.no.risk",
                 "p.tb", "p.tbr", "p.tb.death", "p.death", "p.emigrate")

state.number <- length(state.names)
new.state.names <- c(state.names,
                     paste("V.", state.names, sep = ""),
                     paste("SC.", state.names, sep = ""),
                     paste("FC.", state.names, sep = ""),
                     paste("SQ.", state.names, sep = ""))

# Define argument list for transitions
arglist <- CreateArgumentList(state.names, state.number)

# Run the model for each test/treatment combo and save results
for (test in testlist) {
  for (treat in treatmentlist) {
    
    testing <- test
    treatment <- treat
    strategy <- paste(test, treat, sep = "_")
    
    # Transition matrices
    arglist.S1.TM <- arglist$load.list("S1.TMKD")
    arglist.BASELINE.S1.TM <- arglist$load.list("BASELINE.S1.TMKD")
    
    # State creation
    CreateStates(state.names)
    
    # Define strategies
    S2 <- DefineStrategy(..., transition.matrix = do.call(DefineTransition, arglist.S1.TM))
    S0_12 <- DefineStrategy(..., transition.matrix = do.call(DefineTransition, arglist.BASELINE.S1.TM))
    
    # Define parameters
    parameters <- DefineParameters(...)
    
    # Run model
    result_baseline <- DoRunModel(S0_12, startyear, totalcycles)
    result_strategy <- DoRunModel(S2, startyear, totalcycles)
    
    # Save results
    dir.create("Data/Output", showWarnings = FALSE, recursive = TRUE)
    saveRDS(result_baseline, file = file.path("Data", "Output", paste0("S0_", strategy, ".rds")))
    saveRDS(result_strategy, file = file.path("Data", "Output", paste0("S2_", strategy, ".rds")))
  }
}
