#'===========================================================================================================
#' This script creates a figure with six cost-effectivenes planes,
#' showing incremental cost and effectiveness (in QALYs) of different intervention strategies.
#' To create the rds files that this script can import you need to run to gene the "Results table.R" first
#' to generate the results.
#' 
#' Inputs:
#' Need to run the "Results table.R" first, which creates a couple of rds files that this code then uses.
#' 
#' Output:
#' tiff
#' 
#' Coding style
#' https://google.github.io/styleguide/Rguide.xml

#' LOAD LIBRARIES ===========================================================================================
library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)
# library(extrafont)
# install.packages("extrafont", type = "binary")

#' Need to obtain chance of having sae with different treatment regimens.
#' I have researched this and it is in an excel file in "Model parameters"

#' Set working directory
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Data")

data <- readRDS("offshore_results.rds")
data <- as.data.table(data)
data2 <- readRDS("onshore_results.rds")
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
}

plot1 <- sortformatfunc(off11.35)
plot2 <- sortformatfunc(off11.65)
plot3 <- sortformatfunc(off11.65.200)
plot4 <- sortformatfunc(on11.35)
plot5 <- sortformatfunc(on11.65)
plot6 <- sortformatfunc(on11.65.200)

# Get the colour palatte
getPalette <- brewer.pal(4, "Spectral")
getPalette

ylimmin <- -2
ylimmax <- 8.5
xlimmin <- -5
xlimmax <- 76
linewidth <- 0.5

titleposxaxis <- 45 
titleposyaxis <- 7 
pointsize <- 2
textsize <- 7
textsize2 <- 7


#' I need to use true negative signs and not the hyphens that R automatically uses,
#' therefore I need to create new labels for the x axis.
yaxislab  <- c("\u20132", "0", "2", "4", "6", "8")
xaxislab  <- c("\u20135", "0", "5",  "15", 
               "25",  "35", "45", 
               "55",  "65", "75")
yaxisbreaks  <- c(-2, 0, 2, 4, 6, 8)
xaxisbreaks  <- c(-5, 0, 5, 15,
               25, 35, 45,
               55, 65,  75)

options(scipen=5)
myplot1 <-
  ggplot(plot1, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag =  "D)") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  # annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
  #          label = "Offshore strategy\n11-35 year olds from 100+/100,000ppy") +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour="black"),
        legend.position = "none")
myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag =  "E)") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  # annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
  #          label = "Offshore strategy\n11-65 year olds from 100+/100,000ppy") +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag =  "F)") +
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
  theme(panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
myplot4 <-
  ggplot(plot4, aes(x = incremental.qalys, y = incremental.cost/1000000,
                 fill = strategy,
                 shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag =  "A)") +
  # annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
  #          label = "Onshore strategy\n11-35 year olds from 100+/100,000ppy") +
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
  theme(panel.border = element_blank(),
        #text = element_text(size = textsize2),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
        #legend.position = c(0.80, 0.8),
        #axis.text.x = element_text(angle=45, hjust=1),
myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag = "B)") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  # annotate("text", x = 50, y = titleposyaxis,  size = textsize, 
  #          label = "Onshore strategy\n11-65 year olds from 100+/100,000ppy") +
  scale_y_continuous(breaks = yaxisbreaks,
                     labels = yaxislab) +
  scale_x_continuous(breaks = xaxisbreaks,
                     labels = xaxislab) +
  # theme_set(theme_cowplot(font_size = 8))+
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax), 
                  expand = F) +
  theme(panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())
myplot6 <-
  ggplot(plot6, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 3) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth, lty = 2) +
  labs(x = "Incremental QALYs", 
       y = "Incremental Cost in Millions, A$.",
       fill = "Strategy",
       shape = "Strategy",
       tag =  "C)") +
  # theme_set(theme_cowplot(font_size = 8))+
  # annotate("text", x = titleposxaxis, y = titleposyaxis, size = textsize,  
  #          label = "Onshore strategy\n11-65 year olds from 200+/100,000ppy") +
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
  theme(panel.border = element_blank(),
        axis.text = element_text(colour="black"),
        panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())

g <- ggplotGrob(myplot5 + theme(legend.position = "bottom")+
                  guides(fill = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5),
                         shape = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]


setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
tiff('Figures/ceaplane1.tiff', units = "in", width = 9, height = 5,
     res = 300)
plot_grid(myplot4, myplot5, myplot6, myplot1, myplot2,  myplot3,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1, 1, 1))
          #labels = c("A)", "B)", "C)", "D)", "E)", "F)"))
#labels = c("a)", "b)", "c)", "d)", "e)", "f)", ""),
#label_x = -0.001, label_y = 0.05,
#hjust = 0, vjust = 0)
dev.off()

