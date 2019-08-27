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

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA")
# Reading in the original base ICER
df <- read.xlsx("Model parameters.xlsx", 
                sheetName = "Tornado plot input",
                startRow = 1)
# original value of output
base.value <- as.numeric(as.character(df[1,2]))
# Reading in the rest of the sensitivity analysis data from excel
setwd("H:/Katie/PhD/CEA")
df <- read.xlsx("Model parameters.xlsx", 
                sheetName = "Tornado plot input",
                startRow = 3)
df <- as.data.table(df)
df[, parameter := as.character(parameter)]
setnames(df,"icer.lower.limit","lower")
setnames(df,"icer.upper.limit","upper")
df[, UL_Difference := abs(upper - lower)]

# get order of parameters according to size of intervals
# (I use this to define the ordering of the factors which I then use to define the positions in the plot)
order.parameters <- df %>% arrange(UL_Difference) %>%
  mutate(parameter=factor(x=parameter, levels=parameter)) %>%
  select(parameter) %>% unlist() %>% levels()

# width of columns in plot (value between 0 and 1)
width <- 0.7


setnames(df, "lower", "Change in ICER from base-case using lower value")
setnames(df, "upper", "Change in ICER from base-case using upper value")


# get data frame in shape for ggplot and geom_rect
df.2 <- df %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value', 4:5) %>%
  # just reordering columns
  select(parameter, type, output.value, UL_Difference) %>%
  # create the columns for geom_rect
  mutate(parameter=factor(parameter, levels=order.parameters),
         ymin=pmin(output.value, base.value),
         ymax=pmax(output.value, base.value),
         xmin=as.numeric(parameter)-width/2,
         xmax=as.numeric(parameter)+width/2)


#order.parameters[2] <- "LTBI prevalence and reactivation rate estimates\n(25th percentile LTBI prevalence estimate\nand upper uncertainty limit for reactivation\nrates - 75th percentile LTBI prevalence estimate and\nlower uncertainty limit for reactivation rate)"

# create plot
# (use scale_x_continuous to change labels in y axis to name of parameters)
#png(width = 960, height = 540)
options(scipen=5)

dev.off()
ggplot() + 
  geom_rect(data = df.2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  labs(y = "Cost per QALY (AUS$)") +
  scale_fill_manual(values=c("steelblue2","darksalmon"))+
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.direction="vertical",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(50,50,50,50)) + 
  geom_hline(yintercept = base.value) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters) +
  scale_y_continuous(position = "bottom", breaks = seq(0, 500000, 50000),
                     labels = comma) +
  coord_flip()+
  theme(text = element_text(size=15))

grid.text("Base case ICER: $95,867", x = unit(0.58, "npc"), y = unit(0.2, "npc"))

