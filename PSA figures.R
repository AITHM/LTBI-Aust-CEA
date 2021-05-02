#'===========================================================================================================
#' This script creates a cost-effectivenes plane and acceptibility curves for the probabistic sensitivity analysis.
#' 
#' Inputs:
#' Need to run the "PSA in 4 cors" first, which creates rds files for the onshore, and offshore
#' strategies with the model results for the 1000 simulations
#' 
#' Output:
#' tiff, eps and pdf files
#' 
#' LOAD LIBRARIES ===========================================================================================
options(scipen = 999)
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(epitools)
library(plyr)
library(tidyr)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)
library(extrafont)
# font_import()
loadfonts(device="win")
library(showtext)

# Sourcing required functions from other scripts
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/Git/LTBI-Aust-CEA")
source("CB-TLTBI Functions.R") # contains many functions necessary to run the model
source("CB-TLTBI_DataPreparation.R") # for sorting the population data
# source("Distribution parameter calculations.R") # for determining distribution parameter values

################## PSA #####################################

# Defining the number of simulations we want
Num_SIm <- 1000

# Generating a random set of numbers, one for each simulation
# that will be used as a seed number for "set.seed" functions
# below, so that the sampling from distributions is the same
# for both the baseline and strategy model runs
set.seed(10000)
set.seed.number <- sample(1000:1000000, 10000, replace = F)

# Create a datatable that will eventually contain the
# parameter values to be used for each simulation/model run.
simrun <- c(seq(1, Num_SIm))
simdata <- as.data.frame(simrun)
simdata <- as.data.table(simdata)

# ONSHORE a1 and a2
simrun.output.plot <- readRDS("Data/PSA/onshore.rds")

WTP = 50000 # willingness to pay threshold

plotdata <- cbind(simdata, simrun.output.plot)
plotdata[, incremental.qaly := stratqaly - baseqaly] 
plotdata[, incremental.cost := stratcost - basecost] 
plotdata[, icer := (stratcost - basecost)/(stratqaly - baseqaly)]
plotdata[, dominated := ifelse(incremental.qaly < 0 & incremental.cost > 0, 1, NA)] 
plotdata[, dominant := ifelse(incremental.qaly > 0 & incremental.cost < 0, 1, NA)]
plotdata <- plotdata[, c("incremental.cost", "incremental.qaly", "icer",
                         "dominated", "dominant" )]

plotdata[icer > WTP, wtp.colour := 0] 
plotdata[icer < WTP, wtp.colour := 1]
plotdata[dominated == 1, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer < WTP, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer > WTP, wtp.colour := 1]

widthcm <- 8
heightcm <- 6

#' I need to use true negative signs and not the hyphens that R automatically uses,
#' therefore I need to create new labels for the x axis.
xaxislabon  <- c("\u2013120",
                 "\u201390",
                 "\u201360",
                 "\u201330",
                 "0",  "30", 
                 "60", "90")
xaxisbreakson  <- c(-120, -90, -60, -30,
                    0,  30,  60,
                    90)

yaxislabon  <- c("\u20131", "0",  "2",  "4",  "6",  "8",  "10")
yaxisbreakson  <- c(-1, 0, 2, 4, 6, 8, 10) 

yaxislaboff  <- c("\u20133",
                  "\u20132",
                  "\u20131", "0", "1", "2", "3", "4")
yaxisbreaksoff  <- c(-3, -2, -1, 0, 1, 2, 3, 4) 

xaxislaboff  <- c("\u2013120",
                  "\u201390",
                  "\u201360",
                  "\u201330",
                  "0",  "30", "60", 
                  "90",  "120", "150")
xaxisbreaksoff  <- c(-120, -90, -60, -30,
                     0,  30,  60,
                     90,  120, 150)

yaxislabaccept  <- c("\u201310", "0",  "20",  "40",
                     "60",  "80",  "100")
yaxisbreaksaccept  <- c(-10, 0,  20,  40,  60,  80, 100) 

cut.axis.at <- 300000

plotdata[, wtp.colour := as.factor(wtp.colour)]

# ONSHORE
ylimmin <- -1
ylimmax <- 10.4
xlimmin <- -120
xlimmaxon <- 95

ylimmax.axis <- 10
xlimmax.axis <- 90

pointsize <- 1
textsize2 <- 25

dist <- 10

onshore.a1 <-
  ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000,
                       colour = wtp.colour)) +
  geom_point(size = pointsize, 
             # alpha = 0.5, 
             na.rm = T) +
  # y axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = ylimmin) +
  # dotted y axis
  annotate("segment", x = 0, y = ylimmin, 
           xend = 0, yend = ylimmax.axis,
           lty = 3) +
  # dotted x axis
  annotate("segment", x = xlimmin, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  # 1 QALY/$45,000 trendline - dotted
  annotate("segment", x = -22222222/1000000, y = ylimmin, 
           xend = xlimmax.axis, yend = 4050000/1000000,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline - dashed
  annotate("segment", x = -13333333/1000000, y = ylimmin, 
           xend = xlimmax.axis, yend = 6750000/1000000,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$") +
  # tag = "A)") +
  scale_colour_manual(values = c("black", "black")) +
  scale_y_continuous(breaks = yaxisbreakson,
                     labels = yaxislabon) +
  scale_x_continuous(breaks = xaxisbreakson,
                     labels = xaxislabon) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmaxon), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        axis.text = element_text(colour = "black"))

# Plot of acceptability curve
# works out the proportion cost-effective
plotdata[, cheaper.and.worse := ifelse(incremental.qaly < 0 & incremental.cost < 0, 1, NA)]
cheaper.and.worse.list <- plotdata$icer[plotdata$cheaper.and.worse == 1]
plotdata[, better.and.costly := ifelse(incremental.qaly > 0 & incremental.cost > 0, 1, NA)]
better.and.costly.list <- plotdata$icer[plotdata$better.and.costly == 1]
dominant <- plotdata$dominant
dominated <- plotdata$dominated

maxwtp <- 300000
wtp <- c(0:maxwtp)

propcosteffectivefunc <- function(wtp) {
  ((sum(dominant == 1, na.rm = TRUE) + 
      sum(better.and.costly.list <= wtp, na.rm = TRUE) + 
      sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)) / length(cheaper.and.worse.list)) * 100
}

propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))

# create a new table, the accepty table
accepty <- data.frame(wtp, propcosteffect)

xlimminaccepty <- 0
xlimmaxaccepty <- 319000
ylimminaccepty <- -10
ylimmaxaccepty <- 104

ylimmax.axis <- 100
xlimmax.axis <- 300000

# plot
onshore.a2 <-
  ggplot(accepty, aes(x = wtp, y = propcosteffect))+
  geom_line(size = 1, colour = "black") +
  # y axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmax.axis, yend = ylimminaccepty) + 
  # dotted x axis
  annotate("segment", x = xlimminaccepty, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  labs(x = "Willingness-to-Pay Threshold, A$",
       y = "Cost Effectiveness, %" ) +
  # tag = "B)") +
  scale_x_continuous(label = comma, breaks = seq(0, xlimmaxaccepty, 50000)) +
  scale_y_continuous(breaks = yaxisbreaksaccept,
                     labels = yaxislabaccept) +
  coord_cartesian(xlim = c(xlimminaccepty, xlimmaxaccepty), 
                  ylim = c(ylimminaccepty, ylimmaxaccepty), 
                  expand = F) +
  theme_bw() +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        axis.text = element_text(colour = "black"))








# ONSHORE uLTBI b1 and b2
simrun.output.plot <- readRDS("Data/PSA/onshoreultbi.rds")
# Plot of PSA results on cost effectiveness plane.
# The code below will plot the NumSIm model run outputs on a
# cost effectiveness plane.

WTP = 50000 # willingness to pay threshold

plotdata <- cbind(simdata, simrun.output.plot)
plotdata[, incremental.qaly := stratqaly - baseqaly] 
plotdata[, incremental.cost := stratcost - basecost] 
plotdata[, icer := (stratcost - basecost)/(stratqaly - baseqaly)]
plotdata[, dominated := ifelse(incremental.qaly < 0 & incremental.cost > 0, 1, NA)] 
plotdata[, dominant := ifelse(incremental.qaly > 0 & incremental.cost < 0, 1, NA)]
plotdata <- plotdata[, c("incremental.cost", "incremental.qaly", "icer",
                         "dominated", "dominant" )]

plotdata[icer > WTP, wtp.colour := 0] 
plotdata[icer < WTP, wtp.colour := 1]
plotdata[dominated == 1, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer < WTP, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer > WTP, wtp.colour := 1]

plotdata[, wtp.colour := as.factor(wtp.colour)]

# ONSHORE


xlimmax.axis <- 90
ylimmax.axis <- 10

onshore.b1 <-
  ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000,
                       colour = wtp.colour)) +
  geom_point(size = pointsize, 
             # alpha = 0.5, 
             na.rm = T) +
  # y axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = ylimmin) +
  # dotted y axis
  annotate("segment", x = 0, y = ylimmin, 
           xend = 0, yend = ylimmax.axis,
           lty = 3) +
  # dotted x axis
  annotate("segment", x = xlimmin, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  # 1 QALY/$45,000 trendline - dashed
  annotate("segment", x = -22222222/1000000, y = ylimmin, 
           xend = xlimmax.axis, yend = 4050000/1000000,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline - dotted
  annotate("segment", x = -13333333/1000000, y = ylimmin, 
           xend = xlimmax.axis, yend = 6750000/1000000,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$") +
  # tag = "C)") +
  scale_colour_manual(values = c("black", "black")) +
  scale_y_continuous(breaks = yaxisbreakson,
                     labels = yaxislabon) +
  scale_x_continuous(breaks = xaxisbreakson,
                     labels = xaxislabon) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmaxon), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.border = element_blank(),
        axis.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

# Plot of acceptability curve
# works out the proportion cost-effective
plotdata[, cheaper.and.worse := ifelse(incremental.qaly < 0 & incremental.cost < 0, 1, NA)]
cheaper.and.worse.list <- plotdata$icer[plotdata$cheaper.and.worse == 1]
plotdata[, better.and.costly := ifelse(incremental.qaly > 0 & incremental.cost > 0, 1, NA)]
better.and.costly.list <- plotdata$icer[plotdata$better.and.costly == 1]
dominant <- plotdata$dominant
dominated <- plotdata$dominated

maxwtp <- 300000
wtp <- c(0:maxwtp)


propcosteffectivefunc <- function(wtp) {
  ((sum(dominant == 1, na.rm = TRUE) + 
      sum(better.and.costly.list <= wtp, na.rm = TRUE) + 
      sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)) / length(cheaper.and.worse.list)) * 100
}

propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))

# create a new table, the accepty table
accepty <- data.frame(wtp, propcosteffect)

ylimmax.axis <- 100
xlimmax.axis <- 300000

# plot
onshore.b2 <-
  ggplot(accepty, aes(x = wtp, y = propcosteffect))+
  geom_line(size = 1, colour = "black") +    
  # y axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmax.axis, yend = ylimminaccepty) + 
  # dotted x axis
  annotate("segment", x = xlimminaccepty, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  labs(x = "Willingness-to-Pay Threshold, A$",
       y = "Cost Effectiveness, %") +
  # tag = "D)") +
  scale_x_continuous(label = comma, breaks = seq(0, xlimmaxaccepty, 50000)) +
  scale_y_continuous(breaks = yaxisbreaksaccept,
                     labels = yaxislabaccept) +
  coord_cartesian(xlim = c(xlimminaccepty, xlimmaxaccepty), 
                  ylim = c(ylimminaccepty, ylimmaxaccepty), 
                  expand = F) +
  theme_bw() +
  theme(text = element_text(family = "Arial", size = textsize2),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.grid.minor = element_blank())



tiff('C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/Figure 5.tiff',
     units = "in", width = 19, height = 13,
     res = 300)
plot_grid(onshore.a1, onshore.a2, onshore.b1, onshore.b2,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1),
          labels = c("A)", "B)", "C)", "D)"))
#labels = c("a)", "b)", "c)", "d)"))
dev.off()


setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 3A.eps")
showtext_begin() ## call this function after opening a device
onshore.a1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 3B.eps")
showtext_begin() ## call this function after opening a device
onshore.a2
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 3C.eps")
showtext_begin() ## call this function after opening a device
onshore.b1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 3D.eps")
showtext_begin() ## call this function after opening a device
onshore.b2
dev.off()

pdf("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 3.pdf",
    width = 19, height = 13, paper = 'special')
showtext_begin() ## call this function after opening a device
plot_grid(onshore.a1, onshore.a2, onshore.b1, onshore.b2,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1))

dev.off()










# OFFSHORE uLTBI a1 and a2
simrun.output.plot <- readRDS("Data/PSA/offshore.rds")

# Plot of PSA results on cost effectiveness plane.
# The code below will plot the NumSIm model run outputs on a
# cost effectiveness plane.

WTP = 50000 # willingness to pay threshold

plotdata <- cbind(simdata, simrun.output.plot)
plotdata[, incremental.qaly := stratqaly - baseqaly] 
plotdata[, incremental.cost := stratcost - basecost] 
plotdata[, icer := (stratcost - basecost)/(stratqaly - baseqaly)]
plotdata[, dominated := ifelse(incremental.qaly < 0 & incremental.cost > 0, 1, NA)] 
plotdata[, dominant := ifelse(incremental.qaly > 0 & incremental.cost < 0, 1, NA)]
plotdata <- plotdata[, c("incremental.cost", "incremental.qaly", "icer",
                         "dominated", "dominant" )]

plotdata[icer > WTP, wtp.colour := 0] 
plotdata[icer < WTP, wtp.colour := 1]
plotdata[dominated == 1, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer < WTP, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer > WTP, wtp.colour := 1]

widthcm <- 8
heightcm <- 6

ylimmin <- -3
ylimmax <- 4.2
xlimmin <- -120
xlimmax <- 155
xlimmax.axis <- 150
ylimmax.axis <- 4
pointsize <- 1

# OFFSHORE
offshore.a1 <- 
  ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000)) +
  geom_point(size = pointsize, 
             # alpha = 0.5, 
             na.rm = T) +
  # y axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = ylimmin) +
  # dotted y axis
  annotate("segment", x = 0, y = ylimmin, 
           xend = 0, yend = ylimmax.axis,
           lty = 3) +
  # dotted x axis
  annotate("segment", x = xlimmin, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  # 1 QALY/$45,000 trendline - dotted
  annotate("segment", x = -66.666666, y = ylimmin, 
           xend = 88.88888, yend = ylimmax.axis,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline - dashed
  annotate("segment", x = -40, y = ylimmin, 
           xend = 53.333, yend = ylimmax.axis,
           colour = "gray53",  size = 1, lty = 2) +
  # geom_abline(intercept = 0, slope = (45000/1000000)/1,
  #             colour = "gray53",
  #             size = 1, lty = 3) +
  # geom_abline(intercept = 0, slope = (75000/1000000)/1,
  #             colour = "gray53", 
  #             size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$") +
  # tag = "A)") +
  scale_y_continuous(breaks = yaxisbreaksoff,
                     labels = yaxislaboff) +
  scale_x_continuous(breaks = xaxisbreaksoff,
                     labels = xaxislaboff) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.border = element_blank(),
        axis.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

# Plot of acceptability curve
# works out the proportion cost-effective
plotdata[, cheaper.and.worse := ifelse(incremental.qaly < 0 & incremental.cost < 0, 1, NA)]
cheaper.and.worse.list <- plotdata$icer[plotdata$cheaper.and.worse == 1]
plotdata[, better.and.costly := ifelse(incremental.qaly > 0 & incremental.cost > 0, 1, NA)]
better.and.costly.list <- plotdata$icer[plotdata$better.and.costly == 1]
dominant <- plotdata$dominant
dominated <- plotdata$dominated

maxwtp <- 300000
wtp <- c(0:maxwtp)


# wtp <- 473
# sum(dominant == 1, na.rm = TRUE)
# sum(better.and.costly.list <= wtp, na.rm = TRUE) 
# sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)
# length(cheaper.and.worse.list)

propcosteffectivefunc <- function(wtp) {
  ((sum(dominant == 1, na.rm = TRUE) + 
      sum(better.and.costly.list <= wtp, na.rm = TRUE) + 
      sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)) / length(cheaper.and.worse.list)) * 100
}

propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))

# create a new table, the accepty table
accepty <- data.frame(wtp, propcosteffect)

xlimmax.axis <- 300000
ylimmax.axis <- 100


# plot
offshore.a2 <-
  ggplot(accepty, aes(x = wtp, y = propcosteffect))+
  geom_line(size = 1, colour = "black") +
  labs(x = "Willingness-to-Pay Threshold, A$",
       y = "Cost Effectiveness, %") +
  # tag = "B)") +
  # y axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmax.axis, yend = ylimminaccepty) + 
  # dotted x axis
  annotate("segment", x = xlimminaccepty, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  scale_x_continuous(label = comma, breaks = seq(0, xlimmaxaccepty, 50000)) +
  scale_y_continuous(breaks = yaxisbreaksaccept,
                     labels = yaxislabaccept) +
  coord_cartesian(xlim = c(xlimminaccepty, xlimmaxaccepty), 
                  ylim = c(ylimminaccepty, ylimmaxaccepty), 
                  expand = F) +
  theme_bw() +
  theme(text = element_text(family = "Arial", size = textsize2),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.grid.minor = element_blank())


# OFFSHORE uLTBI b1 and b2
simrun.output.plot <- readRDS("Data/PSA/offshoreultbi.rds")

# Plot of PSA results on cost effectiveness plane.
# The code below will plot the NumSIm model run outputs on a
# cost effectiveness plane.

WTP = 50000 # willingness to pay threshold

plotdata <- cbind(simdata, simrun.output.plot)
plotdata[, incremental.qaly := stratqaly - baseqaly] 
plotdata[, incremental.cost := stratcost - basecost] 
plotdata[, icer := (stratcost - basecost)/(stratqaly - baseqaly)]
plotdata[, dominated := ifelse(incremental.qaly < 0 & incremental.cost > 0, 1, NA)] 
plotdata[, dominant := ifelse(incremental.qaly > 0 & incremental.cost < 0, 1, NA)]
plotdata <- plotdata[, c("incremental.cost", "incremental.qaly", "icer",
                         "dominated", "dominant" )]

plotdata[icer > WTP, wtp.colour := 0] 
plotdata[icer < WTP, wtp.colour := 1]
plotdata[dominated == 1, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer < WTP, wtp.colour := 0]
plotdata[incremental.qaly < 0 & incremental.cost < 0 & icer > WTP, wtp.colour := 1]

xlimmax.axis <- 150
ylimmax.axis <- 4


# OFFSHORE
offshore.b1 <- 
  ggplot(plotdata, aes(x = incremental.qaly, y = incremental.cost/1000000)) +
  geom_point(size = pointsize, 
             # alpha = 0.5, 
             na.rm = T) +
  # y axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = ylimmin) +
  # dotted y axis
  annotate("segment", x = 0, y = ylimmin, 
           xend = 0, yend = ylimmax.axis,
           lty = 3) +
  # dotted x axis
  annotate("segment", x = xlimmin, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  # 1 QALY/$45,000 trendline
  annotate("segment", x = -66.666666, y = ylimmin, 
           xend = 88.88888, yend = ylimmax.axis,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = -40, y = ylimmin, 
           xend = 53.333, yend = ylimmax.axis,
           colour = "gray53",  size = 1, lty = 2) +
  # geom_abline(intercept = 0, slope = (45000/1000000)/1,
  #             colour = "gray53",
  #             size = 1, lty = 3) +
  # # geom_abline(intercept = 0, slope = (100000/1000000)/1,
  # #             colour = "gray55",
  # #             size = 1) +
  # geom_abline(intercept = 0, slope = (75000/1000000)/1,
  #             colour = "gray53", 
  #             size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$") +
  # tag = "C)") +
  scale_y_continuous(breaks = yaxisbreaksoff,
                     labels = yaxislaboff) +
  scale_x_continuous(breaks = xaxisbreaksoff,
                     labels = xaxislaboff) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.border = element_blank(),
        axis.text = element_text(colour = "black"),        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

# Plot of acceptability curve
# works out the proportion cost-effective
plotdata[, cheaper.and.worse := ifelse(incremental.qaly < 0 & incremental.cost < 0, 1, NA)]
cheaper.and.worse.list <- plotdata$icer[plotdata$cheaper.and.worse == 1]
plotdata[, better.and.costly := ifelse(incremental.qaly > 0 & incremental.cost > 0, 1, NA)]
better.and.costly.list <- plotdata$icer[plotdata$better.and.costly == 1]
dominant <- plotdata$dominant
dominated <- plotdata$dominated

maxwtp <- 300000
wtp <- c(0:maxwtp)


# wtp <- 473
# sum(dominant == 1, na.rm = TRUE)
# sum(better.and.costly.list <= wtp, na.rm = TRUE) 
# sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)
# length(cheaper.and.worse.list)

propcosteffectivefunc <- function(wtp) {
  ((sum(dominant == 1, na.rm = TRUE) + 
      sum(better.and.costly.list <= wtp, na.rm = TRUE) + 
      sum(cheaper.and.worse.list >= wtp, na.rm = TRUE)) / length(cheaper.and.worse.list)) * 100
}

propcosteffect <- unlist(lapply(wtp, propcosteffectivefunc))

# create a new table, the accepty table
accepty <- data.frame(wtp, propcosteffect)

xlimmax.axis <- 300000
ylimmax.axis <- 100

# plot
offshore.b2 <-
  ggplot(accepty, aes(x = wtp, y = propcosteffect))+
  geom_line(size = 1, colour = "black") +
  labs(x = "Willingness-to-Pay Threshold, A$",
       y = "Cost Effectiveness, %") +
  # tag = "D)") +
  # y axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmin, yend = ylimmax.axis) +
  # x axis
  annotate("segment", x = xlimminaccepty, y = ylimminaccepty, 
           xend = xlimmax.axis, yend = ylimminaccepty) + 
  # dotted x axis
  annotate("segment", x = xlimminaccepty, y = 0, 
           xend = xlimmax.axis, yend = 0,
           lty = 3) +
  scale_x_continuous(label = comma, breaks = seq(0, xlimmaxaccepty, 50000)) +
  scale_y_continuous(breaks = yaxisbreaksaccept,
                     labels = yaxislabaccept) +
  coord_cartesian(xlim = c(xlimminaccepty, xlimmaxaccepty), 
                  ylim = c(ylimminaccepty, ylimmaxaccepty), 
                  expand = F) +
  theme_bw() +
  theme(text = element_text(family = "Arial", size = textsize2),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        axis.text = element_text(colour = "black"))


tiff('C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/Figure 6.tiff',
     units = "in", width = 19, height = 13,
     res = 300)
plot_grid(offshore.a1, offshore.a2, offshore.b1, offshore.b2,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1),
          labels = c("A)", "B)", "C)", "D)"))
# labels = c("a)", "b)", "c)", "d)"))
dev.off()


setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 4A.eps")
showtext_begin() ## call this function after opening a device
offshore.a1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 4B.eps")
showtext_begin() ## call this function after opening a device
offshore.a2
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 4C.eps")
showtext_begin() ## call this function after opening a device
offshore.b1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 4D.eps")
showtext_begin() ## call this function after opening a device
offshore.b2
dev.off()

pdf("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 4.pdf",
    width = 19, height = 13, paper = 'special')
showtext_begin() ## call this function after opening a device
plot_grid(offshore.a1, offshore.a2, offshore.b1, offshore.b2,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1))
# labels = c("A)", "B)", "C)", "D)"))

dev.off()


