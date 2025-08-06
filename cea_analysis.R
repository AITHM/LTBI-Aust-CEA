# CEA analysis.R (Refactored)

# Required packages assumed loaded by master script

# Access parameter values
attscreen <- get_param("attscreen")
sptst10 <- get_param("sptst10")
sntst10 <- get_param("sntst10")
att <- get_param("att")

# Define a relative path from the script directory to the output folder
output_path <- file.path(this_file_path, "Data", "Output")

# Read in the .rds files from that folder
filenames <- list.files(output_path, pattern = "\\.rds$", full.names = TRUE)
files <- lapply(filenames, function(path) {
  dt <- readRDS(path)
  dt <- as.data.table(dt)
  return(dt)
})

# Create a list of output file names
namelist <- list.files(output_path, pattern = "\\.rds$")
namelist <- gsub("\\.rds$", "", namelist)  # Remove file extension
namelist <- gsub("S2", "", namelist)         # Remove S2
namelist <- substring(namelist, 2)             # Remove first character
files <- setNames(files, namelist)

# Add STRAT and YEAR columns
files <- mapply(function(dt, name) {
  dt[, YEAR := cycle + startyear]
  dt[, STRAT := name]
  return(dt)
}, files, namelist, SIMPLIFY = FALSE)

# Dynamically set baseline strategy from switch or default
baseline_strategy_index <- as.integer(get_switch("baseline_strategy"))
baseline_strategy <- names(files)[baseline_strategy_index]
base <- files[[baseline_strategy]]

# Ensure column p.tb exists before using it
if (!"p.tb" %in% names(base)) stop("Column 'p.tb' not found in baseline data")
if (!"p.tb.death" %in% names(base)) stop("Column 'p.tb.death' not found in baseline data")

basetbcount <- base[, sum(p.tb, na.rm = TRUE)]
basetbdeath <- base[YEAR == finalyear, sum(p.tb.death, na.rm = TRUE)]

# Define the tabfunc() to calculate metrics for a given strategy

tabfunc <- function(dt) {
  dt <- as.data.table(dt)
  strat <- dt$STRAT[1]
  
  # Total cost
  sc_cols <- grep("^SC\\.", names(dt), value = TRUE)
  fc_cols <- grep("^FC\\.", names(dt), value = TRUE)
  dt[, SCsum := rowSums(.SD, na.rm = TRUE), .SDcols = sc_cols]
  dt[, FCsum := rowSums(.SD, na.rm = TRUE), .SDcols = fc_cols]
  totcost <- sum(dt$SCsum) + sum(dt$FCsum)
  
  # Total QALYs
  sq_cols <- grep("^SQ\\.", names(dt), value = TRUE)
  dt[, SQsum := rowSums(.SD, na.rm = TRUE), .SDcols = sq_cols]
  qalytot <- sum(dt$SQsum)
  
  # Incremental values
  totaddcost <- totcost - sum(base$SCsum) - sum(base$FCsum)
  incremqaly <- qalytot - sum(base$SQsum)
  
  # ICER logic
  if (totaddcost < 0 & incremqaly > 0) {
    description <- "cost saving"
    costperqaly <- totaddcost / incremqaly
  } else if (totaddcost > 0 & incremqaly < 0) {
    description <- "dominated"
    costperqaly <- totaddcost / incremqaly
  } else if (totaddcost < 0 & incremqaly < 0) {
    description <- "lower cost and QALY"
    costperqaly <- totaddcost / incremqaly
  } else {
    description <- "ICER"
    costperqaly <- totaddcost / incremqaly
  }
  
  result <- data.table(
    strategy = strat,
    total_cost = totcost,
    total_qalys = qalytot,
    incremental_cost = totaddcost,
    incremental_qalys = incremqaly,
    cost_per_qaly = costperqaly,
    outcome = description
  )
  
  return(result)
}

# Compare all strategies except baseline
comparison_strategies <- setdiff(names(files), baseline_strategy)
table1 <- rbindlist(lapply(comparison_strategies, function(name) {
  dt <- files[[name]]
  tabfunc(dt)
}))

# Write the results to clipboard
write.table(table1, "clipboard", sep = "\t", row.names = FALSE)
