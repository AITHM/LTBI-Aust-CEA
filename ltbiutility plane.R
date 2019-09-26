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

# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# # Reading in the data from excel
# setwd("H:/Katie/PhD/CEA")
# data <- read.xlsx("Model parameters.xlsx", 
#                   sheetName = "CEA plane input",
#                   startRow = 2)

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA/Data")
data <- read.csv("ltbiutility plane.csv")
data <- as.data.table(data)

data <- data[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
setnames(data, 'Incremental.QALYS', "incremental.qalys")
setnames(data, 'total.additional.cost', "incremental.cost")
data[strategy == "0_12...rds", strategy := "Baseline"]


# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

textsize <- 9
options(scipen=5)
dev.off()
myplot1 <- 
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000000,
                 fill = strategy,
                 shape =  strategy)) +
  geom_point(size = 4, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUS$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
            hjust = -0.03, vjust = 1.3, size = textsize, 
            colour = "black") +
  geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
            hjust = 1, vjust = 1.3, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
            hjust = -0.03, vjust = -0.8, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
            hjust = 1, vjust = -0.8, size = textsize, 
            colour = "black") +
  scale_y_continuous(breaks = seq(-10, 25, 2)) +
  theme_bw() +
  coord_cartesian(xlim = c(-800, 200), ylim = c(-3200000/1000000, 13000000/1000000)) +
  theme(text = element_text(size = 27),
        panel.border = element_blank())
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

setwd("H:/Katie/PhD/CEA/Health eco conference")
tiff('ltbiutility.tiff', units = "in", width = 15, height = 7,
     res = 400)
myplot1
dev.off()

