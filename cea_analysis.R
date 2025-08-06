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

# Define the tabfunc() here or source it from a helper file
# ... (tabfunc logic remains unchanged) ...

# Compare all strategies except baseline
comparison_strategies <- setdiff(names(files), baseline_strategy)
table1 <- rbindlist(lapply(comparison_strategies, function(name) {
  dt <- files[[name]]
  tabfunc(dt)
}))

# Write the results to clipboard
write.table(table1, "clipboard", sep = "\t", row.names = FALSE)
