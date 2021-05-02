#'===========================================================================================================
#' This script creates a figure with six cost-effectiveness planes,
#' showing incremental cost and effectiveness (in QALYs) of different intervention strategies.
#' To create the rds files that this script can import you need to run to gene the "Results table.R" first
#' to generate the results.
#' 
#' Inputs:
#' Need to run the "Results table.R" first, which creates a couple of rds files this code then uses.
#' 
#' Output:
#' eps and pdf figures
#' 
#' LOAD LIBRARIES ===========================================================================================
# library(xlsx)
library(data.table)
library(ggplot2)
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

#' Need to obtain chance of having sae with different treatment regimens.
#' I have researched this and it is in an excel file in "Model parameters"

#' Set working directory
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Data")
setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/")

data <- readRDS("Data/offshore_results.rds")
data <- as.data.table(data)
data2 <- readRDS("Data/onshore_results.rds")
data2 <- as.data.table(data2)

off11.35 <- subset(data, age.low == 11 & age.high == 35 & tbincid == "100+")
off11.65 <- subset(data, age.low == 11 & age.high == 65 & tbincid == "100+")
off11.65.200  <- subset(data, age.low == 11 & age.high == 65 & tbincid == "200+")
on11.35 <- subset(data2, age.low == 11 & age.high == 35 & tbincid == "100+")
on11.65 <- subset(data2, age.low == 11 & age.high == 65 & tbincid == "100+")
on11.65.200  <- subset(data2, age.low == 11 & age.high == 65 & tbincid == "200+")

sortformatfunc <- function(dt){
  dt <- as.data.table(dt)
  dt <- dt[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
  setnames(dt, 'Incremental.QALYS', "incremental.qalys")
  setnames(dt, 'total.additional.cost', "incremental.cost")
  dt[1, strategy := "Baseline"]
  dt[strategy == "QTFGIT.3HP", strategy := "QFTGIT.3HP"]
  dt[strategy == "QTFGIT.4R", strategy := "QFTGIT.4R"]
  dt[strategy == "QTFGIT.6H", strategy := "QFTGIT.6H"]
  dt[strategy == "QTFGIT.9H", strategy := "QFTGIT.9H"]
  dt[, strategy := factor(strategy, levels = c("QFTGIT.3HP", "QFTGIT.4R", "QFTGIT.6H", "QFTGIT.9H",
                                               "TST10.3HP", "TST10.4R", "TST10.6H", "TST10.9H",
                                               "TST15.3HP", "TST15.4R", "TST15.6H", "TST15.9H",
                                               "Baseline"))]
  dt[is.na(incremental.cost), incremental.cost := 0]
  dt[is.na(incremental.qalys), incremental.qalys := 0]
  dt <- subset(dt, strategy != "Baseline")
}

plot1 <- sortformatfunc(off11.35)
plot2 <- sortformatfunc(off11.65)
plot3 <- sortformatfunc(off11.65.200)
plot4 <- sortformatfunc(on11.35)
plot5 <- sortformatfunc(on11.65)
plot6 <- sortformatfunc(on11.65.200)


# Set useful plot parameters
ylimmin <- -0
ylimmax <- 8.5
xlimmin <- 0
xlimmax <- 76
linewidth <- 0.5

titleposxaxis <- 45 
titleposyaxis <- 7 
pointsize <- 5
textsize <- 25
textsize2 <- 25

ylimmax.axis <- 8
xlimmax.axis <- 75

dist <- 10

#' I need to use true negative signs and not the hyphens that R automatically uses,
#' therefore I need to create new labels for the x axis.
yaxislab  <- c("\u20131", "0", "2", "4", "6", "8")
xaxislab  <- c("\u20135", "0", "5",  "15", 
               "25",  "35", "45", 
               "55",  "65", "75")
yaxisbreaks  <- c(-1, 0, 2, 4, 6, 8)
xaxisbreaks  <- c(-5, 0, 5, 15,
                  25, 35, 45,
                  55, 65,  75)

options(scipen = 5)
myplot1 <-
  ggplot(plot1, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        axis.text = element_text(colour = "black"),
        legend.position = "none")

myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  # annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
  #          label = "Offshore strategy\n11-65 year olds from 200+/100,000ppy") +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

myplot4 <-
  ggplot(plot4, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  # theme_set(theme_cowplot(font_size = 8)) +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        #text = element_text(size = textsize2),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        legend.position = "none")

myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +  
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.grid.minor = element_blank())
myplot6 <-
  ggplot(plot6, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
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
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 3.3750,
           colour = "gray53",  size = 1, lty = 3) +
  # 1 QALY/$75,000 trendline
  annotate("segment", x = xlimmin, y = ylimmin, 
           xend = xlimmax.axis, yend = 5.6250,
           colour = "gray53",  size = 1, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Thousands, A$",
       fill = expression(paste("", underline('Age Group, years'))),
       shape = expression(paste("", underline('Age Group, years'))))  +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(text = element_text(size = textsize, family = "Arial"),
        panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = dist, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = dist, r = 0, b = 0, l = 0)),
        panel.grid.minor = element_blank())

g <- ggplotGrob(myplot5 + theme(legend.position = "bottom")+
                  guides(fill = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5),
                         shape = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]


setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1A.eps")
showtext_begin() ## call this function after opening a device
myplot4
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1B.eps")
showtext_begin() ## call this function after opening a device
myplot5
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1C.eps")
showtext_begin() ## call this function after opening a device
myplot6
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1D.eps")
showtext_begin() ## call this function after opening a device
myplot1
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1E.eps")
showtext_begin() ## call this function after opening a device
myplot2
dev.off()

setEPS()
postscript("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1F.eps")
showtext_begin() ## call this function after opening a device
myplot3
dev.off()

pdf("C:/Users/Robin/Documents/Katie/PhD/CEA/Am J Epi/Technical review/R/Figures/AJE-01328-2020 Dale Figure 1.pdf",
    width = 19, height = 13, paper = 'special')
showtext_begin() ## call this function after opening a device
plot_grid(myplot4, myplot5, myplot6, myplot1, myplot2,  myplot3,
          nrow = 2,
          rel_widths = c(1, 1, 1, 1, 1, 1))

dev.off()