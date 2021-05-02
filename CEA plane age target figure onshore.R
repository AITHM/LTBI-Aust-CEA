
#'===========================================================================================================
#' This script creates a cost-effectivenes plane, showing different intervention strategies.
#' showing incremental cost and effectiveness (in QALYs) of different intervention strategies.
#' To create the rds files that this script can import you need to run to gene the "CEA plane age target" first
#' to generate the results.
#' 
#' Inputs:
#' Need to run the "CEA plane age target" first, which can be used to create four rds files (onshore, and offshore
#' with and wihout emigration) that this code then uses.
#' 
#' Output:
#' tiff, eps and pdf files
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml

#' LOAD LIBRARIES ===========================================================================================
# library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)
library(extrafont)
# font_import()
loadfonts(device="win")
library(showtext)


#' I need to use true negative signs and not the hyphens that R automatically uses,
#' therefore I need to create new labels for the x axis.
yaxislab  <- c("\u2013500", "0", "1,000", "2,000", "3,000", 
               "4,000", "5,000")
xaxislab  <- c("\u20135", "0", "5",  "10", 
               "15",  "20", "25", 
               "30",  "35", "40", "45")
yaxisbreaks  <- c(-500, 0, 1000, 2000, 3000, 4000, 5000)
xaxisbreaks  <- c(-5, 0, 5, 10,
                  15, 20, 25,
                  30, 35,  40, 45)

# Reading in the data 
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R")
# data <- readRDS("Data/agetarget.rds")
data <- readRDS("Data/agetargetonshore.rds")
data <- as.data.table(data)

# Write the table to clipboard so I can paste it into Excel
# to create the data table associated with the figure.
write.table(data, "clipboard", sep = "\t", row.names = FALSE)

data <- subset(data, strategy != "0_12...rds")

data <- data[, c("age.low", "age.high", "Percentage.of.all.TB.cases.prevented",
                 "Incremental.QALYS", "total.additional.cost")]
setnames(data, "Incremental.QALYS", "incremental.qalys")
setnames(data, "total.additional.cost", "incremental.cost")
setnames(data, "Percentage.of.all.TB.cases.prevented", "tb.prev.percent")

data[ , strategy := do.call(paste, c(.SD, sep = "-")), .SDcols = c("age.low", "age.high")]

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

data$tb.prev.percent <- percent(data$tb.prev.percent)


# This section is required so that the dashes between the numbers are the 
# length required by the journal.
data$strategy <- factor(data$strategy,levels = c("11-19", "20-29", "30-39",
                                                 "40-59", "60-69", 
                                                 "11-35", "11-65", "36-65"))

data[strategy == "11-19", strategy := paste0("\n11", "\U2013", "19")]
data[strategy == "20-29", strategy := paste0("20", "\U2013", "29")]
data[strategy == "30-39", strategy := paste0("30", "\U2013", "39")]
data[strategy == "40-59", strategy := paste0("40", "\U2013", "59")]
data[strategy == "60-69", strategy := paste0("60", "\U2013", "69")]
data[strategy == "11-35", strategy := paste0("11", "\U2013", "35")]
data[strategy == "11-65", strategy := paste0("11", "\U2013", "65")]
data[strategy == "36-65", strategy := paste0("36", "\U2013", "65")]


# Defining useful parameters
textsize <- 24
geomtextsize <- 7

ylimmax <- 5300000/1000
ylimmin <- -500000/1000
xlimmax <- 46
xlimmin <- -5 
linewidth <- 1  

ylimmax.axis <- 5000
xlimmax.axis <- 45

dist <- 10 # distance between axis numbers and text


options(scipen = 5)

myplot1 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = -225, 
           xend = xlimmax.axis, yend = 2025,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = -375, 
           xend = xlimmax.axis, yend = 3375,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years')))) +
  scale_shape_manual(values = c(1,
                                10, 13,
                                8, 11, 15,
                                17, 19)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 1.1, 
                   segment.color = "transparent",
                   size = geomtextsize) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), 
                  ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.border = element_blank())


# Reading in the data without emigration
data <- readRDS("Data/agetargetonshorenoemig.rds")
data <- as.data.table(data)

# Write the table to clipboard so I can paste it into Excel
write.table(data, "clipboard", sep = "\t", row.names = FALSE)

data <- subset(data, strategy != "0_12...rds")
data <- data[, c("age.low", "age.high", "Percentage.of.all.TB.cases.prevented",
                 "Incremental.QALYS", "total.additional.cost")]
setnames(data, "Incremental.QALYS", "incremental.qalys")
setnames(data, "total.additional.cost", "incremental.cost")
setnames(data, "Percentage.of.all.TB.cases.prevented", "tb.prev.percent")

data[ , strategy := do.call(paste, c(.SD, sep = "-")), .SDcols = c("age.low", "age.high")]

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(x, format = format, digits = digits, ...))
}

data$tb.prev.percent <- percent(data$tb.prev.percent)
data$strategy <- factor(data$strategy,levels = c("11-19", "20-29", "30-39",
                                                 "40-59", "60-69", 
                                                 "11-35", "11-65", "36-65"))

# This section is required so that the dashes between the numbers are the 
# length required by the journal.
data[strategy == "11-19", strategy := paste0("11", "\U2013", "19")]
data[strategy == "20-29", strategy := paste0("20", "\U2013", "29")]
data[strategy == "30-39", strategy := paste0("30", "\U2013", "39")]
data[strategy == "40-59", strategy := paste0("40", "\U2013", "59")]
data[strategy == "60-69", strategy := paste0("60", "\U2013", "69")]
data[strategy == "11-35", strategy := paste0("11", "\U2013", "35")]
data[strategy == "11-65", strategy := paste0("11", "\U2013", "65")]
data[strategy == "36-65", strategy := paste0("36", "\U2013", "65")]

# Resave the plot without the legend
myplot2 <-  
  ggplot(data, aes(x = incremental.qalys, y = incremental.cost/1000,
                   fill = strategy,
                   shape =  strategy)) +
  geom_point(size = 7, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = -225, 
           xend = xlimmax.axis, yend = 2025,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = -375, 
           xend = xlimmax.axis, yend = 3375,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  scale_shape_manual(values = c(1,
                                10, 13,
                                8, 11, 15,
                                17, 19)) +
  scale_fill_manual(values = c(getPalette, getPalette)) +
  geom_text_repel (aes(label = tb.prev.percent),
                   hjust = 1.1, 
                   segment.color = "transparent",
                   size = geomtextsize) +
  geom_rect(aes(xmin = 1, xmax = 18, ymin = 2200, ymax = 4980),
            size = 0.5, fill = "#00000000", color = "black") +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), 
                  ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  annotate(geom = "text", x = 9.5, y = 4820, 
           label = expression(paste("", underline('Age Group, years'))),
           color = "black", size = 7) +
  theme(text = element_text(family = "Arial", size = textsize),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.23, 0.68),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.border = element_blank())

plotty <- plot_grid(myplot1, myplot2, ncol = 2, 
                    rel_widths = c(1, 1))

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 2A.eps")
showtext_begin() ## call this function after opening a device
myplot1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 2B.eps")
showtext_begin() ## call this function after opening a device
myplot2
dev.off()


pdf("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 2.pdf",
    width = 17, height = 7, paper = 'special')
showtext_begin() ## call this function after opening a device
plotty
dev.off()


