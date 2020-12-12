# This script creates a figure with six cost-effectivenes planes,
# showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To create the rds files that this script can import
# You need to run to gene the "Results table.R" first
# to generate the results.


library(xlsx)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)

# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# Save the output to file
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

# on11.65 <- subset(data2, age.low == 11 & age.high == 65 & tbincid == "100+")
# dt <- copy(on11.65)

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
# I need 4 fill colours
getPalette <- brewer.pal(4, "Spectral")
getPalette

ylimmin <- -2
ylimmax <- 8.5
xlimmin <- -4
xlimmax <- 79
linewidth <- 0.5

titleposxaxis <- 45 
titleposyaxis <- 8 
pointsize <- 2
textsize <- 3
textsize2 <- 10

#dev.off()

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
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
           label = "Offshore strategy\n11-35 year olds from 100+/100,000ppy") +
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
#axis.text.x = element_text(angle=45, hjust=1),
myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
           label = "Offshore strategy\n11-65 year olds from 100+/100,000ppy") +
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
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
           label = "Offshore strategy\n11-65 year olds from 200+/100,000ppy") +
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
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
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
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = titleposxaxis, y = titleposyaxis,  size = textsize, 
           label = "Onshore strategy\n11-35 year olds from 100+/100,000ppy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
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
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
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
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
  annotate("text", x = 50, y = titleposyaxis,  size = textsize, 
           label = "Onshore strategy\n11-65 year olds from 100+/100,000ppy") +
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank())
myplot6 <-
  ggplot(plot6, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (45000/1000000)/1,
              colour = "gray83",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (75000/1000000)/1,
              colour = "gray83", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (A$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = titleposxaxis, y = titleposyaxis, size = textsize,  
           label = "Onshore strategy\n11-65 year olds from 200+/100,000ppy") +
  scale_shape_manual(values = c(5, 9, 23, 23,
                                1, 10, 21, 21,
                                0, 7, 22, 22,
                                4)) +
  # scale_fill_manual(values = c(getPalette, 
  #                              getPalette,
  #                              getPalette,
  #                              4)) +
  scale_fill_manual(values = c(5, 9, "gray54", "black",
                               1, 10, "gray54", "black",
                               0, 7, "gray54", "black",
                               4)) +
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
  scale_y_continuous(breaks = seq(-10, 250, 2)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")


# grid_arrange_shared_legend <- function(...) {
#   plots <- list(...)
#   g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom")+
#                     guides(fill = guide_legend(ncol = 4),
#                            shape = guide_legend(ncol = 4)))$grobs
#   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#   lheight <- sum(legend$height)
#   grid.arrange(
#     do.call(arrangeGrob, lapply(plots, function(x)
#       x + theme(legend.position = "none"))),
#     legend,
#     ncol = 1,
#     heights = unit.c(unit(1, "npc") - lheight, lheight))
# }

# # dev.off()
# grid_arrange_shared_legend(myplot4, myplot1,
#                            myplot5, myplot2,
#                            myplot6, myplot3,
#                            nrow = 3)
# 
# setwd("H:/Katie/PhD/CEA/MH---CB-LTBI/Figures")
# tiff('ceaplane1.tiff', units = "in", width = 8, height = 8,
#      res = 200)
# grid_arrange_shared_legend(myplot4, myplot1,
#                            myplot5, myplot2,
#                            myplot6, myplot3,
#                            nrow = 3)


g <- ggplotGrob(myplot5 + theme(legend.position = "bottom")+
                  guides(fill = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5),
                         shape = guide_legend(ncol = 4, title.position="top", title.hjust = 0.5)))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
tiff('Figures/ceaplane1.tiff', units = "in", width = 11, height = 11,
     res = 200)
plot_grid(myplot4, myplot1, myplot5, myplot2, myplot6, myplot3, legend,
          nrow = 4, 
          rel_widths = c(1, 1, 1, 1, 1, 1, 1),
          labels = c("A)", "B)", "C)", "D)", "E)", "F)", ""))
          #labels = c("a)", "b)", "c)", "d)", "e)", "f)", ""),
          #label_x = -0.001, label_y = 0.05,
          #hjust = 0, vjust = 0)
dev.off()

