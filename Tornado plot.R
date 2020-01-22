
# This is the script that creates a tornado figure for the sensitivity analysis in
# the cost-effectiveness analysis of LTBI screening and treatment.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# an excel spreadsheet ("Model paramenter") into the sheet names "Sensitivity analysis"
# and then thes results are transferred into the sheet called "Tornado plot", which is read by this script.

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

# Reading in the data from excel using XLSX, which has some sort of glitch
# setwd("H:/Katie/PhD/CEA")
# # Reading in the original base ICER
# df <- read.xlsx("Model parameters.xlsx", 
#                 sheetName = "Tornado plot input",
#                 startRow = 1)
# # original value of output
# base.value <- as.numeric(as.character(df[1,2]))
# # Reading in the rest of the sensitivity analysis data from excel
# setwd("H:/Katie/PhD/CEA")
# df <- read.xlsx("Model parameters.xlsx", 
#                 sheetName = "Tornado plot input",
#                 startRow = 3)

# Reading in the data from excel using XLSX, which has some sort of glitch
setwd("H:/Katie/PhD/CEA/Data")
# Reading in the original base ICER
df <- read.csv("tornado plot.csv")
# original value of output
base.value <- (as.character(df[1,3]))
base.value <-  as.numeric(gsub('\\D+','', base.value))

# Reading in the rest of the sensitivity analysis data from excel
setwd("H:/Katie/PhD/CEA/Data")
df <- read.csv("tornado plot.csv", skip = 2)
df <- as.data.table(df)
df[, X := NULL]
df[, parameter := as.character(parameter)]
setnames(df,"icer.lower.limit","lower")
setnames(df,"icer.upper.limit","upper")
df[, lower := as.numeric(gsub('\\D+','', lower))]
df[, upper := as.numeric(gsub('\\D+','', upper))]
df[, UL_Difference := abs(upper - lower)]
# 
# order.parameters <- df %>% arrange(UL_Difference) 
# 
# order.parametersb <- order.parameters %>% mutate(parameter = factor(x = parameter, levels = parameter)) 
# 
# %>%
#   mutate(parameter = factor(x = parameter, levels = parameter)) %>%
#   select(parameter) %>% unlist() %>% levels()

# get order of parameters according to size of intervals
# (I use this to define the ordering of the factors which 
# I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(parameter = factor(x = parameter, levels = parameter)) %>%
  select(parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.7

setnames(df, "lower", "Change in ICER from base-case using lower value")
setnames(df, "upper", "Change in ICER from base-case using upper value")

# get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  # gather columns Lower_Bound and Upper_Bound into a single 
  # column using gather
  gather(key ='type', value = 'output.value', 2:3) %>%
  # just reordering columns
  select(parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(parameter = factor(parameter, levels = order.parameters),
         ymin = pmin(output.value, base.value),
         ymax = pmax(output.value, base.value),
         xmin = as.numeric(parameter) - width / 2,
         xmax = as.numeric(parameter) + width / 2)

# order.parameters[14] <- "LTBI prevalence (25th-75th percentile) and\nreactivation rate estimates (upper - lower uncertainty limit)"
# order.parameters[5] <- "Proportion of annual TB cases captured\nduring off-shore CXR screening follow-up (4.8 - 12.5%)"
# order.parameters[13] <- "Proportion that began treatment\nwho were effectively treated (50-90%)"
# order.parameters[3] <- "Time to LTBI treatment commencement\nfollowing migration (0 - 3 months)"


# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
#png(width = 960, height = 540)
options(scipen = 5)

# dev.off()
# OFFSHORE SCREENING
myplot1 <- 
  ggplot() + 
  geom_rect(data = df.2, 
            aes(ymax = ymax, ymin = ymin, 
                xmax = xmax, xmin = xmin, fill = type)) +
  geom_text(aes(x = 0, y = base.value),
            size = 3.5, label = "Base case ICER: $58,347") +
  theme_bw() + 
  labs(y = "Cost per QALY (AUS$)") +
  scale_fill_manual(values = c("steelblue2", "darksalmon")) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(50, 50, 50, 50)) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  scale_y_continuous(position = "bottom", 
                     breaks = seq(0, 500000, 50000),
                     labels = comma) +
  coord_flip(ylim = c(0, 210000))+
  theme(text = element_text(size = 12),
        legend.position = c(0.75, 0.1))

# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
# tiff('tornadooffshore.tiff', units = "in", width = 14, height = 6,
#      res = 200)
# myplot1
# dev.off()


# ONSHORE SCREENING
dev.off()
myplot1<- 
  ggplot() + 
  geom_rect(data = df.2, 
            aes(ymax = ymax, ymin = ymin, 
                xmax = xmax, xmin = xmin, fill = type)) +
  theme_bw() + 
  geom_text(aes(x = 0, y = base.value),
            size = 3.5, label = "Base case ICER: $350,327") +
  labs(y = "Cost per QALY (AUS$)") +
  scale_fill_manual(values = c("steelblue2", "darksalmon")) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(50, 50, 50, 50)) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  scale_y_continuous(position = "bottom", 
                     breaks = seq(0, 5000000, 100000),
                     labels = comma) +
  coord_flip(ylim = c(0, 800000))+
  theme(text = element_text(size = 12),
        legend.position = c(0.75, 0.2))


setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
tiff('tornadoonshore.tiff', units = "in", width = 14, height = 6,
     res = 200)
myplot1
dev.off()