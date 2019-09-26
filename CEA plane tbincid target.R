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
data <- read.csv("cea plane tbincid target.csv")
data <- as.data.table(data)
data[, incremental.cost := as.numeric(gsub('\\D+','', incremental.cost))]
data[, icer := as.numeric(gsub('\\D+','', icer))]


# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Reds")
getPalette



data$strategy <- factor(data$strategy, levels = c("40+/100,000", "100+/100,000",
               "150+/100,000", "200+/100,000"))

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...), "%")
}

data$tb.prev.percent <- percent(data$tb.prev.percent)



textsize <- 8
options(scipen = 5)
dev.off()
myplot1 <- 
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
       y = "Incremental cost (AUS$millions)",
       fill = "Target by:\nTB incidence\nin country of birth",
       shape = "Target by:\nTB incidence\nin country of birth") +
  scale_shape_manual(values = c(24, 21,
                              22, 23)) +
  scale_fill_manual(values = c(getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
            hjust = 0.5, vjust = -1,
            segment.color = "transparent",
            size = textsize) +
  geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
            hjust = -0.03, vjust = 1.1, size = textsize, 
            colour = "black") +
  geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
            hjust = 1, vjust = 1.1, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
            hjust = -0.03, vjust = -0.5, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
            hjust = 1, vjust = -0.5, size = textsize, 
            colour = "black") +
  scale_y_continuous(breaks = seq(-20, 25, 5)) +
  scale_x_continuous(breaks = seq(-10, 60, 10)) +
  theme_bw() +
  coord_cartesian(xlim = c(-15, 30), 
                  ylim = c(-5000000/1000000, 28500000/1000000)) +
  theme(text = element_text(size = 27),
        panel.border = element_blank())

setwd("H:/Katie/PhD/CEA/Health eco conference")
tiff('ceaplanetbincid.tiff', units = "in", width = 15, height = 7,
     res = 400)
myplot1
dev.off()
