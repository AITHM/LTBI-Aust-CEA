
#'===========================================================================================================
#' This script creates a tornado figure for the sensitivity analysis in 
#' the cost-effectiveness analysis of LTBI screening and treatment.
#' To create the rds files that this script can import you need to run to gene the "Tornado plot" first
#' to generate the results.The output for this script is then, awkwardly, pasted into Excel and the data is read 
#' in as a csv. Not ideal. 
#' 
#' Inputs:
#' Need to run the "Tornado plot" first, which can be used to create an excel file that this code then uses.
#' 
#' Output:
#' tiff
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml

#' LOAD LIBRARIES ===========================================================================================
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
library(egg)
library(cowplot)
library(grid)
library(gridExtra)

onshore <- 1

#' Reading in the data from excel using XLSX, which has some sort of glitch
setwd("H:/Katie/PhD/CEA/Data")
#' Reading in the original base ICER
df <- read.csv("tornado plot on.csv")
#' original value of output
base.value <- (as.character(df[1,3]))
base.value <- as.numeric(gsub("[^0-9\\-]", "", base.value))

#' Reading in the rest of the sensitivity analysis data from excel
setwd("H:/Katie/PhD/CEA/Data")
df <- read.csv("tornado plot on.csv", skip = 2)
df <- as.data.table(df)
df[, X := NULL]
df[, parameter := as.character(parameter)]
setnames(df,"icer.lower.limit","lower")
setnames(df,"icer.upper.limit","upper")
df[, lower := as.numeric(gsub("[^0-9\\-]", "", lower))]
df[, upper := as.numeric(gsub("[^0-9\\-]", "", upper))]
df[, UL_Difference := abs(upper - lower)]

df <- subset(df, !is.na(parameter))

#' get order of parameters according to size of intervals
#' (I use this to define the ordering of the factors which 
#' I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(parameter = factor(x = parameter, levels = parameter)) %>%
  select(parameter) %>% unlist() %>% levels()

#' width of columns in plot (value between 0 and 1)
width <- 0.7

setnames(df, "lower", "Change in ICER from\nbase-case using lower value")
setnames(df, "upper", "Change in ICER from\nbase-case using upper value")

#' get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  #' gather columns Lower_Bound and Upper_Bound into a single 
  #' column using gather
  gather(key ='type', value = 'output.value', 2:3) %>%
  #' just reordering columns
  select(parameter, type, output.value, UL_Difference) %>%
  #' create the columns for geom_rect
  mutate(parameter = factor(parameter, levels = order.parameters),
         ymin = pmin(output.value, base.value),
         ymax = pmax(output.value, base.value),
         xmin = as.numeric(parameter) - width / 2,
         xmax = as.numeric(parameter) + width / 2)

#' Create plot
options(scipen = 5)

# dev.off()
  #' ONSHORE SCREENING
  myplot1<- 
    ggplot() + 
    geom_rect(data = df.2, 
              aes(ymax = ymax, ymin = ymin, 
                  xmax = xmax, xmin = xmin, fill = type),
              colour="black") +
    theme_bw() + 
    geom_text(aes(x = 0, y = 203188),
              size = 5, label = "Base case ICER: $203,188") +
    labs(y = "Cost per QALY (AUD$)", tag = "A)") +
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
    coord_flip(ylim = c(0, 500000))+
    theme(text = element_text(size = 18),
          legend.position = c(0.76, 0.2))

myplot1


onshore <- 0

#' Reading in the data from excel using XLSX, which has some sort of glitch
setwd("H:/Katie/PhD/CEA/Data")
#' Reading in the original base ICER
df <- read.csv("tornado plot off.csv")
#' original value of output
base.value <- (as.character(df[1,3]))
base.value <- as.numeric(gsub("[^0-9\\-]", "", base.value))

#' Reading in the rest of the sensitivity analysis data from excel
setwd("H:/Katie/PhD/CEA/Data")
df <- read.csv("tornado plot off.csv", skip = 2)
df <- as.data.table(df)
df[, X := NULL]
df[, parameter := as.character(parameter)]
setnames(df,"icer.lower.limit","lower")
setnames(df,"icer.upper.limit","upper")
df[, lower := as.numeric(gsub("[^0-9\\-]", "", lower))]
df[, upper := as.numeric(gsub("[^0-9\\-]", "", upper))]
df[, UL_Difference := abs(upper - lower)]

df <- subset(df, !is.na(parameter))

#' get order of parameters according to size of intervals
#' (I use this to define the ordering of the factors which 
#' I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(parameter = factor(x = parameter, levels = parameter)) %>%
  select(parameter) %>% unlist() %>% levels()

#' width of columns in plot (value between 0 and 1)
width <- 0.7

setnames(df, "lower", "Change in ICER from\nbase-case using lower value")
setnames(df, "upper", "Change in ICER from\nbase-case using upper value")

#' get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  #' gather columns Lower_Bound and Upper_Bound into a single 
  #' column using gather
  gather(key ='type', value = 'output.value', 2:3) %>%
  #' just reordering columns
  select(parameter, type, output.value, UL_Difference) %>%
  #' create the columns for geom_rect
  mutate(parameter = factor(parameter, levels = order.parameters),
         ymin = pmin(output.value, base.value),
         ymax = pmax(output.value, base.value),
         xmin = as.numeric(parameter) - width / 2,
         xmax = as.numeric(parameter) + width / 2)

#' Create plot
options(scipen = 5)

# dev.off()
#' OFFSHORE SCREENING

  myplot2 <- 
    ggplot() + 
    geom_rect(data = df.2, 
              aes(ymax = ymax, ymin = ymin, 
                  xmax = xmax, xmin = xmin, fill = type),
              colour="black") +
    geom_text(aes(x = 0, y = base.value),
              size = 5, label = "Base case ICER: $13,907") +
    theme_bw() + 
    labs(y = "Cost per QALY (AUD$)", tag = "B)") +
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
                       breaks = seq(-20000, 500000, 20000),
                       labels = comma) +
    coord_flip(ylim = c(-20000, 65000))+
    theme(text = element_text(size = 18),
          legend.position = c(0.76, 0.2))


#' Saving the plot 
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")

tiff('Figures/Figure 2.tiff', units = "in", width = 18, height = 12,
     res = 100)
plot_grid(myplot1, myplot2,
          nrow = 2, 
          rel_widths = c(1, 1))
dev.off()

