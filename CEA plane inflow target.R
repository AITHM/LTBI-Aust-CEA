# This script creates a cost-effectivenes plane, showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# an excel spreadsheet ("Model paramenter") into the sheet names "Table 3"
# and then these results are transferred into the sheet called "CEA plane", which is read by this script.

library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# # Reading in the data from excel
# setwd("H:/Katie/PhD/CEA")
# data <- read.xlsx("Model parameters.xlsx", 
#                   sheetName = "CEA plane input",
#                   startRow = 2)

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA/Data")
data <- read.csv("cea plane inflow.csv")
data <- as.data.table(data)
data[, incremental.cost := as.numeric(gsub('\\D+','', incremental.cost))]
data[, icer := as.numeric(gsub('\\D+','', icer))]


# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Greens")
getPalette



data$strategy <- factor(data$strategy, levels = c("one year", "two years",
                                                  "five years", "ten years"))



textsize <- 8
options(scipen = 5)
dev.off()
ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000000,
                 fill = strategy,
                 shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental/additional cost (AUS$millions)",
       fill = "Migrant inflows",
       shape = "Migrant inflows") +
  # scale_size_continuous(limits = c(250, 450), 
  #                       range = c(5, 12), 
  #                       breaks = c(250, 350,
  #                                  450))+
  scale_shape_manual(values = c(24, 21,
                                22, 23)) +
  scale_fill_manual(values = c(getPalette)) +
  geom_text_repel (aes(label = tb.prev),
                   hjust = 0.5, vjust = -1,
                   segment.color = "transparent",
                   size = textsize) +
  geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
            hjust = -0.03, vjust = 1, size = textsize, 
            colour = "black") +
  geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
            hjust = 1, vjust = 1, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
            hjust = -0.03, vjust = -0.7, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
            hjust = 1, vjust = -0.7, size = textsize, 
            colour = "black") +
  scale_y_continuous(breaks = seq(-100, 250, 5)) +
  scale_x_continuous(breaks = seq(-100, 600, 50)) +
  theme_bw() +
  coord_cartesian(xlim = c(-75, 300), 
                   ylim = c(-5000000/1000000, 20000000/1000000)) +
  theme(text = element_text(size = 27),
        panel.border = element_blank())
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

