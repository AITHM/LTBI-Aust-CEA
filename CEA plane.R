# Creates a cost-effectivenes plane, showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.

library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)

# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA")
data <- read.xlsx("Model parameters.xlsx", 
                  sheetName = "CEA plane input",
                  startRow = 2)
data <- as.data.table(data)


# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

textsize <- 7
options(scipen=5)
dev.off()
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
       y = "Incremental/additional cost (AUS$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values=c(19, 24, 24, 24, 24,
                              21, 21, 21, 21,
                              22, 22, 22, 22)) +
  scale_fill_manual(values=c(19, getPalette, 
                             getPalette,
                             getPalette)) +
  geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
            hjust = -0.03, vjust = 3, size = textsize, 
            colour = "black") +
  geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
            hjust = 1, vjust = 3, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
            hjust = -0.03, vjust = -1, size = textsize, 
            colour = "black") +
  geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
            hjust = 1, vjust = -1, size = textsize, 
            colour = "black") +
  scale_y_continuous(breaks = seq(-10, 25, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(-40, 225), ylim = c(-8000000/1000000, 25000000/1000000)) +
  theme(text = element_text(size = 30, family = "TT Arial"),
        panel.border = element_blank())
        #legend.position = c(0.80, 0.8),
        #axis.text.x = element_text(angle=45, hjust=1),

