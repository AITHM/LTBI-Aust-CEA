# This script creates a figure with six cost-effectivenes planes,
# showing incremental cost and effectiveness (in QALYs) of
# different intervention strategies.
# To be used in the cost-effectivness model for latent TB screening and treatment program.
# The files CB-TLTBI.R and CEA analyses need to be run to create the output, which was then added into
# csv spreadsheets of various names, which are read in below

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(egg)
library(cowplot)
library(grid)
library(gridExtra)

# Need to obtain chance of having sae with different treatment regimens.
# I have researched this and it is in an excel file in "Model parameters"

# Load data
setwd("H:/Katie/PhD/CEA/MH---CB-LTBI")
# setwd("C:/Users/Robin/Documents/Katie/PhD/CEA/LTBI-Aust-CEA")


data <- readRDS("Data/cea.plane.2.offshore.rds")

data <- as.data.table(data)


# Write the table to clipboard so I can paste it into Excel
write.table(data, file = "clipboard-16384", sep = "\t", row.names = FALSE)

lifetime <- subset(data, other == "lifetime horizon")
thirtyyrs <- subset(data, other == "30yr horizon")
noemigoff <- subset(data, other == "perm emigration")
ultbioff <- subset(data, other == "LTBI decrement")
inflow30 <- subset(data, other == "30 inflows and horizon")
spec <- subset(data, other == "All specialist")
cascade <- subset(data, other == "Perfect cascade")

sortformatfunc <- function(dt){
  dt <- as.data.table(dt)
  dt <- dt[, c('strategy', 'total.additional.cost', 'Incremental.QALYS')]
  setnames(dt, 'Incremental.QALYS', "incremental.qalys")
  setnames(dt, 'total.additional.cost', "incremental.cost")
  dt[1, strategy := "Baseline"]
  dt[, strategy := as.character(strategy)]
  dt[, strategy := factor(strategy, levels = c("QFTGIT.3HP", "QFTGIT.4R", "QFTGIT.6H", "QFTGIT.9H",
                                               "TST10.3HP", "TST10.4R", "TST10.6H", "TST10.9H",
                                               "TST15.3HP", "TST15.4R", "TST15.6H", "TST15.9H",
                                               "Baseline"))]
  dt[is.na(incremental.cost), incremental.cost := 0]
  dt[is.na(incremental.qalys), incremental.qalys := 0]
}

plot1 <- sortformatfunc(lifetime)
plot2 <- sortformatfunc(thirtyyrs)
plot3 <- sortformatfunc(noemigoff)
plot4 <- sortformatfunc(ultbioff)
plot5 <- sortformatfunc(inflow30)
plot6 <- sortformatfunc(spec)
plot7 <- sortformatfunc(cascade)

# Get the colour palatte
# I need 4 fill colours
getPalette<-brewer.pal(4, "Spectral")
getPalette

ylimmin <- -1
ylimmax <- 10
xlimmin <- -5
xlimmax <- 80

textx <- 40
texty <- 9

linewidth <- 0.5

pointsize <- 2
textsize <- 4
textsize2 <- 10

legend.shapes <- c(24, 24, 24, 24,
                   21, 21, 21, 21,
                   22, 22, 22, 22,
                   4)

legend.colours <- c(getPalette, 
                    getPalette,
                    getPalette,
                    4)

options(scipen=5)
myplot1 <-
  ggplot(plot1, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = textx, y = texty, 
           label = "Lifetime time horizon") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none")
#axis.text.x = element_text(angle=45, hjust=1),
myplot2 <-
  ggplot(plot2, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = textx, y = texty, 
           label = "30 year time horizon") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())
myplot3 <-
  ggplot(plot3, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  annotate("text", x = textx, y = texty, 
           label = "Screening migrants applying for\npermanent visas only") +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
myplot4 <-
  ggplot(plot4, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  # geom_text(aes(label="More costly\nLess effective", x = -Inf, y = Inf),
  #         hjust = -0.03, vjust = 1.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="More costly\nMore effective", x = Inf, y = Inf),
  #         hjust = 1, vjust = 1.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="Less costly\nLess effective", x = -Inf, y = -Inf),
  #         hjust = -0.03, vjust = -0.2, size = textsize,
  #         colour = "black") +
  # geom_text(aes(label="Less costly\nMore effective", x = Inf, y = -Inf),
  #         hjust = 1, vjust = -0.2, size = textsize,
  #         colour = "black") +
  annotate("text", x = -220, y = texty,
           label = "Applying health utility values for those on\nLTBI treatment from Bauer et al 2015") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-1000, 1000, 50)) +
  theme_bw() +
  coord_cartesian(xlim = c(-460, 50), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())
#legend.position = c(0.80, 0.8),
#axis.text.x = element_text(angle=45, hjust=1),

# tiff('Figures/ceaplaneultbi.tiff', units = "in", width = 8, height = 4,
#      res = 200)
# myplot4
# dev.off()


myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  guides(fill = guide_legend(ncol = 4),
         shape = guide_legend(ncol = 4))+
  annotate("text", x = -250, y = 8, 
           label = "Applying health utility values for those on\nLTBIer et al 2015") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-500, 1000, 25)) +
  theme_bw() +
  coord_cartesian(xlim = c(-600, 5), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank())

# Extract legend
grobs <- ggplotGrob(myplot5)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]



myplot6 <-
  ggplot(plot6, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  annotate("text", x = textx, y = texty, 
           label = "Care provided by specialists") +
  scale_y_continuous(breaks = seq(-10, 250, 1)) +
  scale_x_continuous(breaks = seq(-10, 1000, 5)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, xlimmax), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")


myplot7 <-
  ggplot(plot7, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  annotate("text", x = 90, y = texty, 
           label = "Perfect cascade of care") +
  scale_y_continuous(breaks = seq(-100, 250, 1)) +
  scale_x_continuous(breaks = seq(-100, 1000, 25)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, 225), ylim = c(ylimmin, ylimmax)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")



myplot5 <-
  ggplot(plot5, aes(x = incremental.qalys, y = incremental.cost/1000000,
                    fill = strategy,
                    shape =  strategy)) +
  geom_point(size = pointsize, alpha = 1, na.rm = T) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_abline(intercept = 0, slope = (50000/1000000)/1,
              colour = "gray65",
              size = linewidth, lty = 2) +
  geom_abline(intercept = 0, slope = (100000/1000000)/1,
              colour = "gray65", 
              size = linewidth) +
  labs(x = "Incremental QALYs", 
       y = "Incremental cost (AUD$millions)",
       fill = "Strategy",
       shape = "Strategy") +
  annotate("text", x = 220, y = 130, 
           label = "30 year migrant inflow\nand horizon") +
  scale_shape_manual(values = legend.shapes) +
  scale_fill_manual(values = legend.colours) +
  scale_y_continuous(breaks = seq(-40, 500, 20)) +
  scale_x_continuous(breaks = seq(-300, 1000, 50)) +
  theme_bw() +
  coord_cartesian(xlim = c(xlimmin, 470), ylim = c(-10, 140)) +
  theme(text = element_text(size = textsize2),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")



grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom")+
                    guides(fill = guide_legend(ncol = 4),
                           shape = guide_legend(ncol = 4)))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position = "none"), ncol=4)),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}




tiff('Figures/ceaplane2.offshore.tiff', units = "in", width = 17, height = 7,
     res = 200)
plot_grid(myplot1, myplot6, myplot4, myplot7, myplot2, myplot3, myplot5, legend,
          nrow = 2, 
          rel_widths = c(1, 1, 1, 1, 1, 1, 1, 1),
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g", ""))
dev.off()

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
# colour = "black") +


# Sorting the values for the permanent migrant screening 

