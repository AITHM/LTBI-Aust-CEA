
# This is the script that creates a figure for the sensitivity analysis 
# comparing the ICER for different utility decrements associated with LTBI treatment.


library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(scales)
library(ggrepel)
library(scales)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)

# This prevents the model run script (CB-TLTBI.R) from sourcing
# the "parameter values" script for the parameter values, because these
# are, instead, defined below.
parameters.already.set <- 1


# read in parameter list and values, which is defined in the "Parameter creation" script
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO ##################
# params <- readRDS("params onshore.rds")
params <- readRDS("params offshore.rds")
################################## CHOOSE WHETHER ONSHORE OR OFFSHORE SCENARIO #################
################################## CHANGE IN PARAMETER VALUES SCRIPT TOO #################
params <- as.data.table(params)

# Create a datatable that will contain 
# different utility decrements we want 
# to apply
utility.dec <- c(seq(0, 0.1, by = 0.1))
ultbi.dt <- as.data.frame(utility.dec)
ultbi.dt <- as.data.table(ultbi.dt)
ultbi.dt[, utility.dec := as.numeric(utility.dec)]
ultbi.dt[, incremental.qalys := NA]
ultbi.dt[, incremental.qalys := as.numeric(incremental.qalys)]


# The following loops down the rows of the table
# and runs the model with each specified utility decrement.
# Then the output is analysed and entered
# into the ultbi.dt data table.

# ltbi.x <- 3

for(ltbi.x in 1:nrow(ultbi.dt)) {
  
  source("CB-TLTBI Functions.R")
  source("Parameter values.R")
  healthyutility <- params[p == "uhealthy", mid]
  limit <- healthyutility - ultbi.dt[ltbi.x, utility.dec]
  ultbi4R <- limit
  uactivetb <- uactivetb - (uhealthy - ultbi4R)

  # Adjusting the partial LTBI treatment utilities so 
  # they are dependent on the value of
  # the sampled utility for full treatment
  part.utility.dec <- 0.5
  ultbipart4R <- uhealthy - ((uhealthy - ultbi4R) * part.utility.dec)

  # Below runs the data prep
  source("CB-TLTBI_DataPreparation.R")
  # Below runs the model
  parameters.already.set <- 1
  source("CB-TLTBI.R")
  
  # This is the modified script form the cea analysis R file that analyses the 
  # output data from the cost-effectiveness analysis.
  library(tidyverse)
  library(data.table)
  
  # Read in the output files
  filenames <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", 
                          pattern = "*.rds", full.names = TRUE)
  files <- lapply(filenames, readRDS)
  
  # Create a list of the names of the output files
  namelist <- list.files("H:/Katie/PhD/CEA/MH---CB-LTBI/Data/Output", 
                         pattern = "*.rds")
  namelist <- gsub("\\b.rds\\b", "", namelist)
  namelist <- gsub("\\bS2\\b", "", namelist)
  namelist <- substring(namelist, 2)
  
  # Create a column with the YEAR and strategy name within each data table
  counter <- 0
  files <- lapply(files, function(dt) {
    dt <- as.data.table(dt)
    dt[, YEAR :=  cycle + start.year]
    counter <<- counter + 1
    dt[, STRAT := namelist[counter]]
  })
  
  # Name the files in the list
  files <- setNames(files, namelist)
  
  # Finding the baseline quantities
  base <- files[[1]]
  dt <- files[[2]]

    # total baseline QALYS
  a <- which( colnames(base) == "SQ.p.sus" )
  b <- which( colnames(base) == "SQ.p.emigrate" )
  base$SQsum <- rowSums(base[, a:b], na.rm = TRUE)
  qalybase <- sum(base$SQsum)
  
  # total number of QALYS
  a <- which( colnames(dt) == "SQ.p.sus" )
  b <- which( colnames(dt) == "SQ.p.emigrate" )
  dt$SQsum <- rowSums(dt[, a:b], na.rm = TRUE)
  qalytot <- sum(dt$SQsum)
  
  #Incremental QALYs
  incremqaly <- qalytot - qalybase
  
  ultbi.dt[ltbi.x, incremental.qalys := incremqaly]
  
}



# # Write the table to clipboard so I can paste it into Excel
# write.table(ultbi.dt, "clipboard", sep = "\t", row.names = FALSE)



ultbi.dt[, utility := uhealthy - utility.dec]





# Create plot
options(scipen=5)

# Function that estimates where th eline intercepts the x-axis
f2 <- approxfun(ultbi.dt$incremental.qalys, ultbi.dt$utility.dec*100)
f2(0)



#dev.off()
myplot1 <-
  ggplot(ultbi.dt, aes(x = utility.dec*100, y = incremental.qalys)) +
  geom_line(size = 1, color = "steelblue2") +
  geom_point(aes(x = f2(0),
                 y = 0), colour = "black", size = 12,
             shape = 1) + # this adds a point at the intercept
  theme_bw() +
  geom_text(aes(x = f2(0), y = 0),
                   hjust = 0.5, vjust = -1,
                   size = 10, label = "0.67%") +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  labs(y = "Incremental QALYS",
       x = "LTBI treatment utility decrement relative to healthy state (%)") +
  # scale_fill_manual(values = c("steelblue2", "darksalmon")) +
  scale_y_continuous(breaks = seq(-60, 100, 5),
                     labels = comma) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-20, 20)) +
  theme(text = element_text(size = 20),
        panel.border = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = "grey"))


tiff('Figures/ltbiutility.tiff', units = "in", width = 16, height = 6,
     res = 100)
myplot1
dev.off()

