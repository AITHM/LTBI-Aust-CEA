# This script creates a figure with six cost-effectivenes planes,
# showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# csv spreadsheets of various names, which are read in below

library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# # Reading in the data from excel
# setwd("H:/Katie/PhD/CEA")
# data <- read.xlsx("Model parameters.xlsx", 
#                   sheetName = "CEA plane input",
#                   startRow = 2)

# Reading in the data from excel
setwd("H:/Katie/PhD/CEA/Data")
noemigoff <- read.csv("cea plane off_noemig.csv")
noemigon <- read.csv("cea plane on_noemig.csv")
ultbioff <- read.csv("cea plane off_ultbi.csv")
ultbion <- read.csv("cea plane on_ultbi.csv")

sortformatfunc <- function(dt){
  dt <- as.data.table(dt)
  dt <- dt[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
  setnames(dt, 'Incremental.QALYS', "incremental.qalys")
  setnames(dt, 'total.additional.cost', "incremental.cost")
  dt[strategy == "0_12...rds", strategy := "Baseline"]
  #dt[, incremental.cost := as.numeric(gsub('\\D+','', incremental.cost))]
  dt[is.na(incremental.cost), incremental.cost := 0]
  dt[is.na(incremental.qalys), incremental.qalys := 0]
  dt[, incremental.qalys := as.character(incremental.qalys)]
  dt[, incremental.qalys := as.numeric(incremental.qalys)]
}

plot1 <- sortformatfunc(noemigoff)
plot2 <- sortformatfunc(noemigon)
plot3 <- sortformatfunc(ultbioff)
plot4 <- sortformatfunc(ultbion)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

ylimmin <- -5
ylimmax <- 18
xlimmin <- -500
xlimmax <- 40


textsize <- 4
textsize2 <- 10

options(scipen=5)
dev.off()
myplot1 <-
  ggplot(plot1, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = 4, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="100+/100,000; 11-35 year olds", x = Inf, y = Inf),
  #                     hjust = "middle", vjust = "middle", size = textsize,
  #                     colour = "black") +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
#           colour = "black") +
# geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
#           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 5)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),
myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = 4, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 5)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = 4, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 5)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
myplot4 <-
  ggplot(plot4, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = 4, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "grey",
              size = 1.5) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(24, 24, 24, 24,
                                21, 21, 21, 21,
                                22, 22, 22, 22, 19)) +
  scale_fill_manual(values = c(getPalette, 
                               getPalette,
                               getPalette, 19)) +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #           hjust = -0.03, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #           hjust = 1, vjust = 1.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #           hjust = -0.03, vjust = -0.2, size = textsize, 
  #           colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #           hjust = 1, vjust = -0.2, size = textsize, 
#           colour = "black") +
scale_y_continuous(breaks = seq(-10, 250, 5)) +
  scale_x_continuous(breaks = seq(-500, 1000, 100)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

dev.off()
ggarrange(myplot1, myplot3,
          myplot2, myplot4, nrow = 2)


setwd("H:/Katie/PhD/CEA/Health eco conference")
tiff('ceaplane1.tiff', units = "in", width = 10, height = 5,
     res = 400)
myplot1
dev.off()